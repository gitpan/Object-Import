use warnings; no warnings qw"uninitialized reserved prototype"; use strict;
use Test::More tests => 23;

BEGIN { 
$::W = 0; $::W1 = 0;
$SIG{__WARN__} = sub { 
	my($t) = @_;
	$::W++;
	if ($t =~ /Subroutine G1::greet redefined/) {
		$::W1++;
	} else {
		warn $t;
	}
};
}

is($::W, 0, "no warn 0");

{
package X;
sub greet {
	my($o, $i) = @_;
	(ref($o) ? $$o[0] : $o) . ", " . $i;
}
}


{
package Hi;
BEGIN { @Hi::ISA = X::; }
}

{
package G0;
use Test::More;

ok(defined(\&greet), "G0 def&greet");
is(greet("world"), "hello, world", "G0 &greet");

use Object::Import bless(["hello"], X::), list => ["greet"];

is($::W, 0, "no warn G0");
}

{
package G1;
use Test::More;

ok(!exists(&greet), "G1 !exi&greet");
my $v = eval q'no strict; greet';
is($@, "", "G1 greet bare err");
is($v, "greet", "G1 greet bare");
ok(defined(&thank), "G1 def&thank");

import Object::Import bless(["hey"], X::), list => ["greet"];

ok(defined(&greet), "G1.1 def&greet");
is(greet("world"), "hey, world", "G1.1 &greet");
$v = eval q'no strict; greet';
is($@, "", "G1.1 greet err");
is($v, "hey, ", "G1.1 greet");
$v = eval q'no strict; greet world';
is($@, "", "G1.1 greet w err");
is($v, "hey, world", "G1.1 greet w");
is($::W, 0, "no warn G1.1");

import Object::Import Hi::, list => ["greet"], nowarn_redefine => 1; 

ok(defined(&greet), "G1.2 def&greet");
is(greet("perl"), "Hi, perl", "G1.2 &greet");
is($::W, 0, "no warn G1.2");

import Object::Import bless(["welcome"], Hi::), list => ["greet"]; 

ok(defined(&greet), "G1.3 def&greet");
is(greet("perl"), "welcome, perl", "G1.3 &greet");
is($::W, 1, "warn G1.3");
is($::W1, 1, "warnt G1.3");

use Object::Import bless(["hullo"], X::), list => ["thank"];
}

is($::W, 1, "warn \$");

__END__
