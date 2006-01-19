#!/usr/bin/perl -w

# $Id: before_start.t,v 1.1 2005-11-28 16:06:51+11 sh1517 Exp sh1517 $

# $Log: before_start.t,v $
# Revision 1.1  2005-11-28 16:06:51+11  sh1517
# Initial revision
#

use Test;

use Nagios::Report ;


# Each element in this array is a single test. Storing them this way makes
# maintenance easy, and should be OK since perl should be pretty functional
# before these tests are run.

$tests = <<'EOTESTS' ;
# Scalar expression 
# 1==1,

($s, $m, $h, $d, $mm, $y)=localtime; $now="$d-" . ($mm +1) . '-' . ($y + 1900) . "  $h:$m:$s"; $t=time(); $si=Nagios::Report::before_start($now, $t - 1); $si == 0
$si = Nagios::Report::before_start($now, $t + 3_600); $si == 1
$si = Nagios::Report::before_start($now, $t - 3_600); $si == 0


EOTESTS

@t = split /\n/, $tests ;
@tests = grep !( m<\s*#> or m<^\s*$> ), @t ;

plan tests => scalar(@tests) ;
# plan tests => scalar(@tests) + 1 ;


for ( @tests ) {

  $sub = eval "sub { $_ }" ;

  warn "sub { $_ } fails to compile: $@"
    if $@ ;

  ok $sub  ;

  1 ;
}

