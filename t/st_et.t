#!/usr/bin/perl -w

# $Id: st_et.t,v 1.3 2005-11-30 22:14:41+11 sh1517 Exp sh1517 $

# $Log: st_et.t,v $
# Revision 1.3  2005-11-30 22:14:41+11  sh1517
# 1 Add more tests for the time patterns based on 'at' times
#   under the __DEFAULT__ tag. All of these tests are hard to
#   understand (and prob wrong).
#
# Revision 1.2  2005-11-28 10:28:13+11  sh1517
# 1 Add tests for 'lastnhours' and 'lastndays'.
#
# Revision 1.1  2005-11-12 20:09:22+11  sh1517
# Initial revision
#

use Test;

use Nagios::Report ;
use Time::Local ;
use constant {
               SEC   => 0,
               MIN   => 1,
               HOUR  => 2,
               MDAY  => 3,
               MON   => 4,
               YEAR  => 5,
               WDAY  => 6,
               YDAY  => 7,
               ISDST => 8,
};



# Each element in this array is a single test. Storing them this way makes
# maintenance easy, and should be OK since perl should be pretty functional
# before these tests are run.

$tests = <<'EOTESTS' ;
# Scalar expression 
# 1==1,

@t = localtime; ($t1, $t2) = $st_et{today}->('', \@t); $t2 - $t1 == time() - timelocal(0, 0, 0, $t[MDAY], $t[MON], $t[YEAR])
($t1, $t2) = $st_et{last24hours}->('', \@t);          $t2 - $t1 == 1 * 86_400
($t1, $t2) = $st_et{last12hours}->('', \@t);          $t2 - $t1 == 1 * 86_400 >> 1
($t1, $t2) = $st_et{__DEFAULT__}->('last0hours', \@t);$t2 - $t1 == 0
($t1, $t2) = $st_et{__DEFAULT__}->('last2hours', \@t);$t2 - $t1 == 2 * 3_600
($t1, $t2) = $st_et{__DEFAULT__}->('last2days', \@t); $t2 - $t1 == 2 * 86_400
# Set @t to whatever time/date to compare with the arg of $st_et.
# The st_et functions return the interval between sometime before now, the first arg, 
# and 'now', the second arg.
@t = localtime(timelocal(0, 30, 20, $t[MDAY], $t[MON], $t[YEAR])); ($t1, $t2) = $st_et{__DEFAULT__}->('20:30', \@t); $t2 - $t1 == 0
@t = localtime(timelocal(0, 30, 20, $t[MDAY], $t[MON], $t[YEAR])); ($t1, $t2) = $st_et{__DEFAULT__}->('2030', \@t);  $t2 - $t1 == 0
@t = localtime(timelocal(0, 0, 0, $t[MDAY], $t[MON], $t[YEAR]) + 86_400); $d = substr("0$t[MDAY]", -2, 2) . "." . substr("0" . ($t[MON] + 1), -2, 2) . "." . (1900+$t[YEAR]); ($t1, $t2) = $st_et{__DEFAULT__}->($d, \@t);  $t2 - $t1 == 0
@t = localtime(timelocal(0, 0, 0, $t[MDAY], $t[MON], $t[YEAR]) + 86_400); $d = substr("0" . ($t[MON] + 1), -2, 2) . "/" . substr("0$t[MDAY]", -2, 2) . "/" . (1900+$t[YEAR]); ($t1, $t2) = $st_et{__DEFAULT__}->($d, \@t);  $t2 - $t1 == 0
@t = localtime(timelocal(0, 30, 20, $t[MDAY], $t[MON], $t[YEAR])); $d = '20:30 ' . substr("0$t[MDAY]", -2, 2) . "." . substr("0" . ($t[MON] + 1), -2, 2) . "." . (1900+$t[YEAR]); ($t1, $t2) = $st_et{__DEFAULT__}->($d, \@t);  $t2 - $t1 == 0
@t = localtime(timelocal(0, 30, 20, $t[MDAY], $t[MON], $t[YEAR])); $d = '2030 ' . substr("0" . ($t[MON] + 1), -2, 2) . "/" . substr("0$t[MDAY]", -2, 2) . "/" . (1900+$t[YEAR]); ($t1, $t2) = $st_et{__DEFAULT__}->($d, \@t);  $t2 - $t1 == 0

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

