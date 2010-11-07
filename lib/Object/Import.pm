package Object::Import;
use warnings; 
use strict;
our $VERSION = 1.001;


=head1 NAME

Object::Import - import methods of an object as functions to a package

=head1 SYNOPSIS

	use Object::Import $object;
	foo(@bar); # now means $object->foo(@bar);

=head1 DESCRIPTION

This module lets you call methods of a certain object more easily by
exporting them as functions to a package.  The exported functions are
not called as methods and do not receive an object argument, but instead
the object is fixed at the time you import them with this module.

You use the module with the following syntax:

	use Object::Import $object, %options;

Here, C<$object> is the object from which you want to import the methods.
This can be a perl object (blessed reference), or the name of a package
that has class methods.  

As usualy, a C<use> statement is executed in compile time, so you should
take care not to use values that you compute only in run-time, eg.

	my $object = Foo::Bar->new();
	use Object::Import $object; # WRONG: $object is not yet initialized

Instead, you have to create the object before you import, such as

	use Object::Import Foo::Bar->new();

You can also call import in run-time, eg.

	use Object::Import ();
	my $object = Foo::Bar->new();
	import Object::Import $object;
	
but in that case, you can't call the imported functions without parenthesis.

If you don't give an explicit list of methods to export, Object::Import
tries to find out what callable methods the object has and import
all of them.  Some methods are excluded from exporting in this case,
namely any methods where exporting would overwrite a function existing
in the target package or would override a builtin function, also
any methods with names that are special to perl, such as C<DESTROY>,
and any methods whose name starts with an underscore.  This automatic
search for methods is quite fragile because of the way perl OO works,
so it can find subroutines that shouldn't actually be called as methods,
or not find methods that can actually be called.  In particular, even
if you import an object from a purely object oriented module, it can
find non-method subs imported from other (non-OO) modules.

If you do give a list of methods to export, Object::Import trusts you
know what you mean, so it exports all those subs even if it has to
replace existing subs or break something else.

=head1 OPTIONS

The following import options can be passed to the module.

=over

=item C<< list => >> I<$arrayref>

Sets the list of methods to export, instead of the module deciding automatically.
I<$arrayref> must be a reference to an array containing method names.  Eg.

	use Object::Import LWP::UserAgent->new, list => 
		[qw"get post head mirror request simple_request"];

=item C<< target => >> I<$package_name>

Export the sub names to the given namespace.  Default is the package
from where you call import.

=item C<< deref => 1 >>

Signals that the first import argument, instead of being the object
itself, is a reference to a scalar that contains the object.

The content of this scalar may later be changed, and the imported
functions will be called on the new contents.  (The scalar may even be
filled with undef, as long as you don't call the functions at that time.)
If you don't pass the list of methods explicitly, the content of the
scalar at the time of the import is used for determining the methods as
a template to determine the methods.  If, however, you give the list
of methods, the content of the scalar is not examined at the time of
the import.

This is useful not only for applying methods to multiple objects, but
for being able to free the object when it's no longer needed.  If you
don't use this option, then any imported function will forever reference
the object.

=item C<< prefix => >> I<$string>

Prepends a string to the names of functions imported.  This is useful if
some of the method names are the same as existing subs or builtins.  Eg.

	use Object::Import $object, prefix => "foo";
	foo_bar(); # calls $object->bar();

=item C<< suffix => >> I<$string>

Like the prefix option, only the string is appended.

=item C<< underscore => 1 >>

Consider a method for automatic inclusion even if its name starts with
an underscore.  Such methods are normally excluded, because they are
usually used as private subs.

=item C<< exclude_methods => >> I<$hashref>

Sets a list of additional methods that are not automatically imported.
The argument must be a reference to a hash whose keys are potential
method names.  Ignored if you use the C<list> option.

=item C<< exclude_imports => >> I<$hashref>

Sets a list of additional sub names which the module must never use as
names of imported subs.  These names are thus compared not with the
original method names, but the names possibly transformed by adding
prefixes and suffixes.  This applies even if you give an explicit C<list>
of methods to import.

=item C<< savenames => >> I<$hashref>

Save the (unqualified) names of the functions exported by adding them
as a key to a hash (the value is incremented with the ++ operator).
This could be useful if you wanted to reexport them with Exporter.
I<$arrayref> must be a real reference to a hash, not an undef.

=item C<< nowarn_redefine => 1 >>

Do not warn when an existing sub is redefined.  That is currently only
possible if you give the list of methods to be exported explicitly with
the C<list> option, because if the module chooses automatically then it
will not redefine subs.

=item C<< nowarn_nomethod => 1 >>

Suppress the warning when you try to import methods from an object that
is either the name of a nonexistent package, or a string that is not
a valid package name, or an unblessed object.  Such values don't have
any methods.  That warning often indicates that you passed the wrong
value to Object::Import or forgot to require a package.

=item C<< debug => 1 >>

Print debugging messages about what the module exports.

=back

=head1 NOTES

=head2 Importing from IO handles

It is possible to use an IO handle as the object to export methods from.
If you do this, you should require IO::Handle first so that the handle
actually has methods.  You should probably also use the prefix or suffix
option in such a case, because many methods of handles have the same name
as a builtin function.  Also warns when the object is an empty string,
undef, an unblessed reference, or other value on which calling methods
does not make sense.

The handle must not be a symbolic reference, whether qualified or 
unqualified, eg.

	open FOO, "<", "somefile" or die;
	use Object::Import "FOO"; # WRONG

You can pass a handle as a glob, reference to glob, or an IO::Handle
object, so any of these would work as the object after the above open
statement: C<*FOO>, C<\*FOO>, C<*FOO{IO}>.  Another way to pass an
IO::Handle object would be like this:

	use IO::File;
	use Object::Import IO::File->new("somefile", "<");

=head1 EXAMPLES

First a simple example of importing class methods.

	use Math::BigInt; 
	use Object::Import Math::BigInt::; 
	warn new("0x100");

This prints 256, because Math::BigInt->new("0x100") creates a big integer equal to 256.

Now let's see a simple example of importing object methods.

	use Math::BigInt; 
	use Object::Import Math::BigInt->new("100"); 
	warn bmul(2); 
	warn as_hex();

This prints 200 (2 multiplied by 100), then 0xc8 (100 as hexadecimal).

Now let's see a more complicated example.  This prints the leading news from the English
Wikinews website.

	use warnings; use strict;
	use LWP::UserAgent; 
	use XML::Twig;
	use Object::Import LWP::UserAgent->new; 
	my $response = get "http://en.wikinews.org/wiki/Special:Export?".
		"pages=Template:Lead_article_1&limit=1";
	import Object::Import $response;
	if (is_success()) {
		use Object::Import XML::Twig->new;
		parse content();
		for my $parmname (qw"title summary") {
			first_elt("text")->text =~ /\|\s*$parmname\s*=([^\|\}]+)/ or die;
			print $1;
		}
	} else {
		die message();
	}

For example, as I am writing this (2010-09-05), this outputs

=over

Magnitude 7.0 earthquake hits New Zealand

An earthquake with magnitude 7.0 occurred near South Island, New 
Zealand at Saturday 04:35:44 AM local time (16:35:44 UTC). The 
earthquake occurred at a depth of 16.1 kilometers (10.0 miles). The 
earthquake was reported to have caused widespread damage and power 
outages. Several aftershocks were also reported.

=back

In this, C<get> refers to the useragent object; C<is_success>, C<content>
and C<message> refers to the response object (and these must be called
with a parenthesis); while C<parse> and C<first_elt> refer to the
twig object.  This is not a good example to follow: it's quite fragile,
and not only because of the simple regex used to parse out the right
parts, but because if a new sub is added to a future version of the
L<LWP::UserAgent> or L<HTTP::Response> classes, they might suddenly get
imported and would shadow the methods we're supposed to import later.

Now let's see an example of using a suffix.

	use File::Temp; 
	use Object::Import scalar(File::Temp->new()), suffix => "temp"; 
	printtemp "hello, world\nhidden"; 
	seektemp 0, 0; 
	print getlinetemp; 
	warn filenametemp;

Here we need the suffix because print and seek are names of builtin
functions.  

This example shows how to export to a different namespace.
This is useful if you want to write your own
sugar module that provides a procedural syntax:

	package My::Object::DSL;
	use Object::Import;
	use My::Object;
	
	sub import {
	    my ($class, %options);
	    if (@_ == 2) {
		($class, $options{ name }) = @_;
	    } else {
		($class, %options) = @_;
	    };
	    my $target = delete $options{ target } || caller;
	    my $name = delete $options{ name } || '$obj';
	    my $obj = My::Object->new(%options);
	    
	    $name =~ s/^[\$]//
		or croak 'Variable name must start with $';
	    {
		no strict 'refs';
		*{"$target\::$name"} = \$obj;
		# Now install in $target::
		import Object::Import \${"$target\::$name"},
				      deref => 1,
				      target => $target;
	    }
        }

You can use the module C<< My::Object::DSL >> as follows:

        use My::Object::DSL '$obj';

If you want to pass more options, you can use 

        use My::Object::DSL name => '$obj', foo => 'bar';

Implementing a small C<::DSL> module instead of using
C<Object::Import> directly has the advantage that you can add defaults
in C<DSL.pm>.

=head1 SEE ALSO

L<Class::Exporter>

=head1 BUGS

Please report bugs using the CPAN bug tracker (under the distribution
name Object-Import), or, failing that, to C<ambrus@math.bme.hu>.

=head1 CREDITS

The primary author and maintainer of this module is Zsban Ambrus
C<ambrus@math.bme.hu>.  Some of the code was written by Max Maischein, who
also gave the motivation to turn a prototype to the full module you see.
Thanks to exussum0 for the original inspiration.  

=head1 COPYING

Copyright (C) Zsban Ambrus 2010

This program is free software: you can redistribute it and/or modify
it under the terms of either the GNU General Public License version 3, 
as published by the Free Software Foundation; or the "Artistic License"
which comes with perl.  

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

A copy of the GNU General Public License can be found in the
source tree of this module under the name "GPL", or else see
"http://www.gnu.org/licenses/".  A copy of the Artistic License can
be found in the source tree under the name "ARTISTIC", or else see
"http://cpansearch.perl.org/src/JESSE/perl-5.12.2/pod/perlartistic.pod".

=cut


use Scalar::Util qw"blessed reftype";
eval "
use MRO::Compat;
"; 
if (my $use_mro_compat_error = $@) {
	eval "
	use mro;
	"; 
	my $use_mro_error = $@;
	$use_mro_error and 
		die "$use_mro_compat_error\n$use_mro_error\nerror: could not use either of modules MRO::Compat or mro";
}


# Methods must not be exported automatically if their original name is in %special_source
# or if the name of the exported sub is in %special_target.  
our %special_source;
our %special_target;

# Any name starting with a character other than a letter or underscore are forced to 
# package main.  Such names in other packages may only be accessed with an explicit 
# package name.  Most of these are special or reserved to be special by the core, though
# none of their function slots are used.  We do not export these because the user could 
# not call them easily unless exported to main.  Note that names starting with unicode 
# non-letter characters or names that start with invalid utf-8 also seem to be forced
# to main (these may only be accessed through symbolic references).
# The following names are also forced to main like above. 
$special_source{$_}++, $special_target{$_}++ for 
	qw"ENV INC ARGV ARGVOUT SIG STDIN STDOUT STDERR _";
# The following names are called by the core on some occasions.
$special_source{$_}++, $special_target{$_}++ for qw"
	AUTOLOAD BINMODE CLEAR CLEARERR CLONE CLONE_SKIP CLOSE DELETE DESTROY
	EOF ERROR EXISTS EXTEND FDOPEN FETCH FETCHSIZE FILENO FILL FIRSTKEY
	FLUSH GETC NEXTKEY OPEN POP POPPED PRINT PRINTF PUSH PUSHED READ READLINE
	SCALAR SEEK SETLINEBUF SHIFT SPLICE STORE STORESIZE SYSOPEN TELL TIEARRAY
	TIEHANDLE TIEHASH TIESCALAR UNREAD UNSHIFT UNTIE UTF8 WRITE";
# Names starting with "(" are used by the overload mechanism, even as functions in some
# cases.  We do not touch such subs.
# The following names are called by use/no, so they definitely should not be exported.
$special_source{$_}++, $special_target{$_}++ for qw"import unimport";
# The following should not occur as subs, but we exclude them for good measure.
$special_source{$_}++, $special_target{$_}++ for 
	qw"BEGIN UNITCHECK CHECK INIT END";
# The following names could override a builtin function if exported to a module
$special_target{$_}++ for qw"
	abs accept alarm atan2 bind binmode bless break caller chdir chmod
	chomp chop chown chr chroot close closedir connect continue cos
	crypt dbmclose dbmopen default defined delete die do dump each
	else elsif endgrent endhostent endnetent endprotoent endpwent
	endservent eof eval exec exists exit exp fcntl fileno flock for
	foreach fork format formline getc getgrent getgrgid getgrnam
	gethostbyaddr gethostbyname gethostent getlogin getnetbyaddr
	getnetbyname getnetent getpeername getpgrp getppid getpriority
	getprotobyname getprotobynumber getprotoent getpwent getpwnam
	getpwuid getservbyname getservbyport getservent getsockname
	getsockopt given glob gmtime goto grep hex if index int
	ioctl join keys kill last lc lcfirst length link listen local
	localtime lock log lstat map mkdir msgctl msgget msgrcv msgsnd
	my next no not oct open opendir ord our pack package pipe pop
	pos print printf prototype push quotemeta rand read readdir
	readline readlink readpipe recv redo ref rename require reset
	return reverse rewinddir rindex rmdir say scalar seek seekdir
	select semctl semget semop send setgrent sethostent setnetent
	setpgrp setpriority setprotoent setpwent setservent setsockopt
	shift shmctl shmget shmread shmwrite shutdown sin sleep socket
	socketpair sort splice split sprintf sqrt srand stat state
	study sub substr symlink syscall sysopen sysread sysseek system
	syswrite tell telldir tie tied time times truncate uc ucfirst
	umask undef unless unlink unpack unshift untie until use utime
	values vec wait waitpid wantarray warn when while write
";
# The following four are UNIVERSAL functions.
$special_source{$_}++, $special_target{$_}++ for qw"can isa DOES VERSION"; 
# The following keywords cannot be overriden this way, so are safe to export,
# though you may have to use tricky syntax to call some of them:
0 for qw "and cmp eq ge gt le lt m ne or q qq qr qw qx s tr x xor y";
# The old aliases LT etc are removed from core at perl 5.8 and do not count
# as special anymore.
# Some of the above long list might also not be overridable, eg. "if".
# The following are special, but are not functions and not forced to main.
0 for qw"a b DATA OVERLOAD";
# The following names are English aliases for special variables so they could
#   be aliased to special names, eg. if the module imports English 
# then &ARG and &::_ are the same.  The function slot of none of these is special.
# Exporting to such names would be a bad idea because they could overwrite
# a function in main.
$special_source{$_}++, $special_target{$_}++ for qw"
	ACCUMULATOR ARG ARRAY_BASE BASETIME CHILD_ERROR COMPILING DEBUGGING
	EFFECTIVE_GROUP_ID EFFECTIVE_USER_ID EGID ERRNO EUID EVAL_ERROR
	EXCEPTIONS_BEING_CAUGHT EXECUTABLE_NAME EXTENDED_OS_ERROR FORMAT_FORMFEED
	FORMAT_LINES_LEFT FORMAT_LINES_PER_PAGE FORMAT_LINE_BREAK_CHARACTERS
	FORMAT_NAME FORMAT_PAGE_NUMBER FORMAT_TOP_NAME GID INPLACE_EDIT
	INPUT_LINE_NUMBER INPUT_RECORD_SEPARATOR LAST_MATCH_END LAST_MATCH_START
	LAST_PAREN_MATCH LAST_REGEXP_CODE_RESULT LAST_SUBMATCH_RESULT
	LIST_SEPARATOR MATCH NR OFMT OFS OLD_PERL_VERSION ORS OSNAME OS_ERROR
	OUTPUT_AUTOFLUSH OUTPUT_FIELD_SEPARATOR OUTPUT_RECORD_SEPARATOR PERLDB
	PERL_VERSION PID POSTMATCH PREMATCH PROCESS_ID PROGRAM_NAME REAL_GROUP_ID
	REAL_USER_ID RS SUBSCRIPT_SEPARATOR SUBSEP SYSTEM_FD_MAX UID WARNING";
# The following are names used by Exporter, but not as functions.
0 for qw"EXPORT EXPORT_OK EXPORT_FAIL EXPORT_TAGS";
# The following are subs used by Exporter, some internal.
$special_source{$_}++, $special_target{$_}++ for qw"
	_push_tags _rebuild_cache as_heavy export export_fail export_fail_in
	export_ok_tags export_tags export_to_level heavy_export
	heavy_export_ok_tags heavy_export_tags heavy_export_to_level
	heavy_require_version require_version";
# (Ideally we should have a mechanism to exclude everything that's defined in Exporter
# or Exporter::Heavy)
# The following are depreciated aliases to the standard filehandles, but as these aren't 
# forced to main we shan't exclude them.
0 for qw"stdin stdout stderr";
# Yeah, these lists got out of hand, but I want a place to collect all special names.
# TODO: See also the B::Keywords module, and submit patches for it.
# If the user gives an list of names, we assume they know what they are doing.

sub special_source {
	my($n) = @_;
	utf8::decode($n);
	exists($special_source{$n}) || $n !~ /\A[_\pL]/;
}
sub special_target {
	my($n) = @_;
	utf8::decode($n);
	exists($special_target{$n}) || $n !~ /\A[_\pL]/;
}


# this returns a list to the methods we want to export automatically
sub list_method {
	my($obj, $expkg, $debug, $nowarn_nomethod, $underscore, $exclude) = @_;
	my $oobj = $obj;
	my %exclude; if ($exclude) { %exclude = %$exclude; }
	my $complain = sub {
		my($k) = @_;
		$nowarn_nomethod and return;
		no warnings "uninitialized";
		warn "warning: Object::Import cannot find methods of " . $k . ": " . $oobj;
	};
	if (reftype($obj) ? !defined(blessed($obj)) && "GLOB" eq reftype($obj) : "GLOB" eq reftype(\$obj)) {
		$obj = *$obj{IO}; # this magically converts any filehandle (glob, ref-to-glob, symref, true handle object) to a handle object.  we need this to find the methods.
		# note that we don't enter here if we have a blessed globref: magical overloaded objects such as File::Temp or Coro::Handle objs can take care of themselves, and we'd lose methods if we dereferenced them to their underlying handles.
		if (!defined($obj)) {
			&$complain("globref with no IO handle");
			return;
		}
	}
	eval { $obj->can("import") }; 
	my $can_methods = !$@; # false if $obj is an unblessed ref or a string that does not look like a package name, so perl refuses to call any methods
	if (!$can_methods) {
		&$complain(
			reftype($obj) ? (defined(blessed($obj)) ? "strange object" : "unblessed reference") :
			!defined($obj) ? "undefined value" :
			!length($obj) ? "empty string value" :
			!$obj ? "false value" :
			"string value that is an invalid package name");
		return;
	} 
	if (!reftype($obj) && do { no strict "refs"; !%{$obj . "::"} }) {
		&$complain("nonexistent package");
	}
	my %r;
	my $class = blessed($obj) || $obj;
	my @class = @{mro::get_linear_isa($class)};
	$debug and warn "debug: Object::Import object $oobj, class $class, search path: @class";
	for my $pkgn (@class) {
		my $pkg = do { no strict "refs"; \%{$pkgn . "::"}};
		for my $m (sort keys %$pkg) {
			if (
				!$exclude{$m} &&
				!$r{$m} &&
				$obj->can($m) && # was exists(&{$$pkg{$m}})
				!special_source($m) &&
				($underscore || $m !~ /\A_/)
			) {
				$r{$m}++;
			}
		}
	}
	keys(%r);
}


sub dor ($$) { 
	my($x, $y) = @_;
	defined($x) ? $x : $y;
}

sub import {
	my($_u, $arg1, @opt) = @_;
	if (@_ <= 1) {
		return; # required for later imports
	}
	0 == @opt % 2 or 
		die q"error: odd number of import options to Object::Import; usage: use Object::Import $obj, %opts";
	my %opt = @opt;
	my($deref, $methl, $debug, $nowarn_redefine, $nowarn_nomethod, $underscore, $exclude_method, $exclude_import, $savename, $funprefix, $funsuffix, $expkgn) = 
		delete(@opt{(qw"deref list debug nowarn_redefine nowarn_nomethod underscore exclude_methods exclude_imports savenames prefix suffix target")});
	%opt and
		die "error: unused import options to Object::Import: " . join(" ", keys(%opt));
	$expkgn = dor($expkgn, scalar caller);
	my $objr = $deref ? $arg1 : \$arg1;
	$_ = dor($_, "") for $funprefix, $funsuffix; # one could use the suffix "0" afterall
	my %exclude_import; $exclude_import and %exclude_import = %$exclude_import; 
	my $expkgns = $expkgn . "::";
	my $expkg = do {no strict 'refs'; \%{$expkgns} };
	if ($debug) { warn "debug: Object::Import starting to export methods to package $expkgns"; }
	my @meth;
	if ($methl) {
		@meth = @$methl;
	} else {
		@meth = list_method do { no strict "refs"; $$objr }, $expkg, $debug, $nowarn_nomethod, $underscore, $exclude_method;
	}
	my @funn;
	for my $methn (@meth) {
		my $funn = $funprefix . $methn . $funsuffix;
		if (!$exclude_import{$funn} &&
			($methl ||
				(!special_target($funn) &&
				!exists(&{$expkgns . $funn}))) # was (!$$expkg{$funn} || !exists(&{$$expkg{$funn}}))
				# that's wrong because of some shortcut symbol table entries for constants or predeclared subs
		) {
			my $p = sub (@) { do { no strict "refs"; $$objr }->${\$methn}(@_) };
			{ 
				no strict 'refs'; 
				if ($nowarn_redefine) {
					no warnings "redefine";
					*{$expkgns . $funn} = $p; 
				} else {
					*{$expkgns . $funn} = $p; 
				}
			}
			push @funn, $funn;
		}
	}
	if ($debug) { warn "debug: Object::Import exported the following functions: ", join(" ", sort(@funn)); }
	if ($savename) {
		$$savename{$_}++ for @funn;
	}
}


1;
__END__
