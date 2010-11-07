use warnings; use strict;
use Test::More tests => 21;

use Object::Import;

BEGIN { 
	$::W = 0; $::W1 = 0;
	$SIG{__WARN__} = sub { 
		my($t) = @_;
		$::W++;
		if ($t =~ /\Awarning: Object::Import cannot find methods of /) 
			{ $::W1++; } 
		else
			{ warn $t; }
	};
}


{ no strict "refs"; *{"=k::greet"} = sub { "hello" }; }
sub greet { "sorry" };

is($::W, 0, "no warn 0");

{
package G0;
use Test::More;

for my $testrec (
	[["hello"], "unblessed ref"],
	["hello", "nonexistant package"],
	["!", "invalid string"],
	["", "empty string"],
	["=k", "package with wrong name"],
) {
	my($obj, $desc) = @$testrec;
	my @n;
	
	import Object::Import $obj, savenames => \@n;

	is_deeply(\@n, [], "no import from $desc");
	ok(!exists(&greet), "G0.1 !exi&greet from $desc");
	is($::W, 1, "warn $desc");
	is($::W1, 1, "warnt $desc");
	$::W = $::W1 = 0;
}
}

__END__
