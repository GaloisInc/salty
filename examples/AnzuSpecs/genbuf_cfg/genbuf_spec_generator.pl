#! /usr/bin/perl -w 

###############################################
# perl script to generated LTL specification and
# config file to synthesize a generalized buffer
# between k senders and 2 receivers.
#
# Usage: ./genbuf_generator.pl <num_of_senders> <prefix>
#
# Generated files: prefix.ltl prefix.cfg
# Default prefix is genbuf
#
###############################################
use strict;
use POSIX; # qw(ceil floor);

sub slc {
    my $j = shift;
    my $bits = shift;
    my $assign = "";
    my $val;

    for (my $bit = 0; $bit < $bits; $bit++) {
	$val = $j % 2;
	$assign .= "SLC$bit=$val";
	$assign .= " * " unless ($bit == $bits-1);
	$j = floor($j/2);
    }
    return $assign;
}

###############################################
# MAIN

if(! defined($ARGV[0])) {
    print "Usage: ./genbuf_generator.pl <num_of_senders> <prefix>\n";
    exit;
}

my $prefix = "genbuf";
if( defined($ARGV[1])) {
    $prefix = $ARGV[1];
}

#variables for LTL specification
my $num_senders = $ARGV[0];
my $slc_bits = ceil((log $num_senders)/(log 2));
$slc_bits = 1 if ($slc_bits == 0);
my $num_receivers = 2;
my $guarantees = "";
my @assert;
my $assumptions = "";
my @assume;

#variables for config file
my $input_vars  = "";
my $output_vars = "";
my $env_initial = "";
my $sys_initial = "";
my $env_transitions = "";
my $sys_transitions = "";
my $env_fairness = "";
my $sys_fairness = "";

###############################################
# Communication with senders
#
my ($g, $a);
for (my $i=0; $i < $num_senders; $i++) {
    #variable definition
    $input_vars .= "StoB_REQ$i;\n";
    $output_vars.= "BtoS_ACK$i;\n";

    #initial state
    $env_initial .= "StoB_REQ$i=0;\n";
    $sys_initial .= "BtoS_ACK$i=0;\n";
    
    $guarantees .= "\n##########################################\n";
    $guarantees .= "#Guarantees for sender $i\n";
    
    ##########################################
    # Guarantee 1
    $g = "G(StoB_REQ$i=1 -> F(BtoS_ACK$i=1));\t#G1\n";
    $guarantees .= $g; push (@assert, $g);
    # Guarantee 2
    $g = "G((StoB_REQ$i=0 * X(StoB_REQ$i=1)) -> X(BtoS_ACK$i=0));\t#G2\n";
    $guarantees .= $g; push (@assert, $g);
    $sys_transitions .= $g;
    $sys_fairness .= "G(F(StoB_REQ$i=1 <-> BtoS_ACK$i=1));\t#G1+G2\n";
    
    # Guarantee 3
#    $g = "G(StoB_REQ$i=0 -> (BtoS_ACK$i=1 + X(BtoS_ACK$i=0)));\t#G3\n";
    $g = "G((BtoS_ACK$i=0 * StoB_REQ$i=0) -> X(BtoS_ACK$i=0));\t#G2\n";
    $guarantees .= $g; push (@assert, $g);
    $sys_transitions .= $g;
    
    # Guarantee 4
    $g = "G((BtoS_ACK$i=1 * StoB_REQ$i=1) -> X(BtoS_ACK$i=1));\t#G4\n";
    $guarantees .= $g; push (@assert, $g);
    $sys_transitions .= $g;
    
    # Assumption 1
    $a = "G((StoB_REQ$i=1 * BtoS_ACK$i=0) -> X(StoB_REQ$i=1));\t#A1\n";
    $assumptions .= $a; push (@assume, $a);
    $env_transitions .= $a;
    $a = "G(BtoS_ACK$i=1 -> X(StoB_REQ$i=0));\t#A1\n";
    $assumptions .= $a; push (@assume, $a);
    $env_transitions .= $a;
    
    # Guarantee 5
    for (my $j=$i+1; $j < $num_senders; $j++) {
	$g = "G((BtoS_ACK$i=0) + (BtoS_ACK$j=0));\t#G5\n";
	$guarantees .= $g; push (@assert, $g);
	$sys_transitions .= $g;
    }
}

###############################################
# Communication with receivers
#
if ($num_receivers != 2) {
    print "Note that the DBW for Guarantee 7 works only for two receivers.\n";
    exit;
}
for (my $j=0; $j < $num_receivers; $j++) {
    #variable definition
    $input_vars .= "RtoB_ACK$j;\n";
    $output_vars.= "BtoR_REQ$j;\n";

    #initial state
    $env_initial .= "RtoB_ACK$j=0;\n";
    $sys_initial .= "BtoR_REQ$j=0;\n";
    
    $guarantees .= "\n##########################################\n";
    $guarantees .= "# Guarantees for receiver $j\n";
    
    # Assumption 2
    $a = "G(BtoR_REQ$j=1 -> F(RtoB_ACK$j=1));\t#A2\n";
    $assumptions .= $a; push (@assume, $a);
    $env_fairness .= "G(F(BtoR_REQ$j=1 <-> RtoB_ACK$j=1));\t#A2\n";
    
    # Assumption 3
    $a = "G(BtoR_REQ$j=0 -> X(RtoB_ACK$j=0));\t#A3\n";
    $assumptions .= $a; push (@assume, $a);
    $env_transitions .= $a;
    
    # Assumption 4
    $a = "G((BtoR_REQ$j=1 * RtoB_ACK$j=1) -> X(RtoB_ACK$j=1));\t#A4\n";
    $assumptions .= $a; push (@assume, $a);
    $env_transitions .= $a;

    # Guarantee 6
    $g = "G((BtoR_REQ$j=1 * RtoB_ACK$j=0) -> X(BtoR_REQ$j=1));\t#G6\n";
    $guarantees .= $g; push (@assert, $g);
    $sys_transitions .= $g;

    # Guarantee 7
    for (my $k=$j+1; $k < $num_receivers; $k++) {
	$g = "G((BtoR_REQ$j=0) + (BtoR_REQ$k=0));\t#G7\n";
	$guarantees .= $g; push (@assert, $g);
	$sys_transitions .= $g;
    }
    # G7: rose($j) -> X (no_rose W rose($j+1 mod $num_receivers))
    my $n = ($j + 1)%$num_receivers; #next
    my $rose_j  = "(BtoR_REQ$j=0 * X(BtoR_REQ$j=1))";
    my $nrose_j = "(BtoR_REQ$j=1 + X(BtoR_REQ$j=0))";
    my $rose_n  = "(BtoR_REQ$n=0 * X(BtoR_REQ$n=1))";
    $g = "G(  $rose_j ->\n X(($nrose_j U $rose_n) + \n G($nrose_j)));\t#G7\n";
    $guarantees .= $g; push (@assert, $g);
    #construct DBW for G7 - see below
    
    # Guarantee 6 and 8
    $g = "G(RtoB_ACK$j=1 -> X(BtoR_REQ$j=0));\t#G8\n";
    $guarantees .= $g; push (@assert, $g);
    $sys_transitions .= $g;
}

# DBW for guarantee 7
$output_vars .= "stateG7_0;\n";
$output_vars .= "stateG7_1;\n";
$sys_initial .= "stateG7_0=0;\n";
$sys_initial .= "stateG7_1=1;\n";
$sys_transitions .= "G((BtoR_REQ0=1 * BtoR_REQ1=1) -> FALSE);\t#G7\n";
$sys_transitions .= "G((stateG7_1=0 * BtoR_REQ0=0 * BtoR_REQ1=1) ->";
$sys_transitions .= " X(stateG7_1=1 * stateG7_0=0));#G7\n";
$sys_transitions .= "G((stateG7_1=1 * BtoR_REQ0=1 * BtoR_REQ1=0) ->";
$sys_transitions .= " X(stateG7_1=0 * stateG7_0=0));#G7\n";
$sys_transitions .= "G((stateG7_1=0 * BtoR_REQ0=0 * BtoR_REQ1=0) ->";
$sys_transitions .= " X(stateG7_1=0 * stateG7_0=1));#G7\n";
$sys_transitions .= "G((stateG7_1=1 * BtoR_REQ0=0 * BtoR_REQ1=0) ->";
$sys_transitions .= " X(stateG7_1=1 * stateG7_0=1));#G7\n";
$sys_transitions .= "G((stateG7_1=0 * stateG7_0=0 * BtoR_REQ0=1 * BtoR_REQ1=0) ->\n";
$sys_transitions .= " X(stateG7_1=0 * stateG7_0=0));\t#G7\n";
$sys_transitions .= "G((stateG7_1=1 * stateG7_0=0 * BtoR_REQ0=0 * BtoR_REQ1=1) ->\n";
$sys_transitions .= " X(stateG7_1=1 * stateG7_0=0));\t#G7\n";
$sys_transitions .= "G((stateG7_1=0 * stateG7_0=1 * BtoR_REQ0=1) -> FALSE);\t#G7\n";
$sys_transitions .= "G((stateG7_1=1 * stateG7_0=1 * BtoR_REQ1=1) -> FALSE);\t#G7\n";

###############################################
# Communication with FIFO and multiplexer
#
#variable definition
$input_vars .= "FULL;\n";
$input_vars .= "EMPTY;\n";
$output_vars .= "ENQ;\n";
$output_vars .= "DEQ;\n";
$output_vars .= "stateG12;\n";

#initial state
$env_initial .= "FULL=0;\n";
$env_initial .= "EMPTY=1;\n";
$sys_initial .= "ENQ=0;\n";
$sys_initial .= "DEQ=0;\n";
$sys_initial .= "stateG12=0;\n";

for (my $bit=0; $bit < $slc_bits; $bit++) {
    $output_vars .= "SLC$bit;\n";
    $sys_initial .= "SLC$bit=0;\n";
}

$guarantees .= "\n##########################################\n";
$guarantees .= "# Guarantees for FIFO and multiplexer\n";

# Guarantee 9: ENQ and SLC
$guarantees .= "\n##########################################\n";
$guarantees .= "# ENQ <-> Exists i: rose(BtoS_ACKi)\n";
my $roseBtoS  = "";
my $roseBtoSi = "";
for (my $i=0; $i < $num_senders; $i++) {
    $roseBtoSi = "(BtoS_ACK$i=0 * X(BtoS_ACK$i=1))";
    $g = "G($roseBtoSi -> X(ENQ=1));\t#G9\n";
    $guarantees .= $g; push (@assert, $g);
    $sys_transitions .= $g;
    
    $roseBtoS   .=   "(BtoS_ACK$i=1 + X(BtoS_ACK$i=0))";
    $roseBtoS   .= " *\n   " if ($i < ($num_senders - 1));
    if ($i == 0) {
	$g = "G($roseBtoSi  -> X(".slc($i, $slc_bits)."));\t#G9\n";
    } else {
	$g = "G($roseBtoSi <-> X(".slc($i, $slc_bits)."));\t#G9\n";
    }
    $guarantees .= $g; push (@assert, $g);
    $sys_transitions .= $g;
}
$g = "G(($roseBtoS) -> X(ENQ=0));\t#G9\n";
$guarantees .= $g; push (@assert, $g);
$sys_transitions .= $g;

# Guarantee 10
$guarantees .= "\n##########################################\n";
$guarantees .= "# DEQ <-> Exists j: fell(RtoB_ACKj)\n";
my $fellRtoB = "";
for (my $j=0; $j < $num_receivers; $j++) {
    $g = "G((RtoB_ACK$j=1 * X(RtoB_ACK$j=0)) -> X(DEQ=1));\t#G10\n";
    $guarantees .= $g; push (@assert, $g);
    $sys_transitions .= $g;
    $fellRtoB   .=   "(RtoB_ACK$j=0 + X(RtoB_ACK$j=1))";
    $fellRtoB   .= " *\n   " if ($j < ($num_receivers - 1));
}
$g = "G(($fellRtoB) -> X(DEQ=0));\t#G10\n";
$guarantees .= $g; push (@assert, $g);
$sys_transitions .= $g;

# Guarantee 11
$guarantees .= "\n";
$g = "G((FULL=1 * DEQ=0) -> ENQ=0);\t#G11\n";
$guarantees .= $g; push (@assert, $g);
$sys_transitions .= $g;

$g = "G(EMPTY=1 -> DEQ=0);\t#G11\n";
$guarantees .= $g; push (@assert, $g);
$sys_transitions .= $g;

# Guarantee 12
$g = "G(EMPTY=0 -> F(DEQ=1));\t#G12\n";
$guarantees .= $g; push (@assert, $g);
$sys_transitions .= "G((stateG12=0 * EMPTY=1) -> X(stateG12=0));\t#G12\n";
$sys_transitions .= "G((stateG12=0 * DEQ=1  ) -> X(stateG12=0));\t#G12\n";
$sys_transitions .= "G((stateG12=0 * EMPTY=0 * DEQ=0) -> X(stateG12=1));\t#G12\n";
$sys_transitions .= "G((stateG12=1 * DEQ=0  ) -> X(stateG12=1));\t#G12\n";
$sys_transitions .= "G((stateG12=1 * DEQ=1  ) -> X(stateG12=0));\t#G12\n";
$sys_fairness .= "G(F(stateG12=0));\t#G12\n";

# Assumption 4
$a = "G((ENQ=1 * DEQ=0) -> X(EMPTY=0));\t#A4\n";
$assumptions .= $a; push (@assume, $a);
$env_transitions .= $a;

$a = "G((DEQ=1 * ENQ=0) -> X(FULL=0));\t#A4\n";
$assumptions .= $a; push (@assume, $a);
$env_transitions .= $a;

$a  = "G((ENQ=1 <-> DEQ=1) -> ((FULL=1 <-> X(FULL=1)) *\n";
$a .= "                        (EMPTY=1 <-> X(EMPTY=1))));\t#A4\n";
$assumptions .= $a; push (@assume, $a);
$env_transitions .= $a;

###############################################
# PRINT LTL FILE
###############################################
print "Generating $prefix.ltl\n";
open (LTL, ">$prefix.ltl");

print LTL "##########################################\n";
print LTL "# Assumptions\n";
print LTL "##########################################\n";
print LTL $assumptions;

print LTL "\n##########################################\n";
print LTL "# Guarantees\n";
print LTL "##########################################\n";
print LTL $guarantees;

close LTL;

###############################################
# PRINT LTL FILE with ASSUMPTIONS
###############################################
print "Generating $prefix-assume.ltl\n";
open (LTL, ">$prefix-assume.ltl");
print LTL "##########################################\n";
print LTL "# Assumptions\n";
print LTL "##########################################\n";
$a=0;
my $stmt = "";
foreach (@assume) {
    s/;//;
    print LTL "\\define A$a $_";
    $stmt .= " * " unless ($a eq 0);
    $stmt .= "\\A$a";
    $a++;
}
print LTL "\\define assume ($stmt)\n";

print LTL "\n##########################################\n";
print LTL "# Guarantees\n";
print LTL "##########################################\n";
foreach (@assert) {
    print LTL "\\assume -> $_";
}
close LTL;
  
###############################################
# PRINT CONFIG FILE
###############################################
print "Generating $prefix.cfg\n";
open (CFG, ">$prefix.cfg");

print CFG "###############################################\n";
print CFG "# Input variable definition\n";
print CFG "###############################################\n";
print CFG "[INPUT_VARIABLES]\n";
print CFG $input_vars;
print CFG "\n";

print CFG "###############################################\n";
print CFG "# Output variable definition\n";
print CFG "###############################################\n";
print CFG "[OUTPUT_VARIABLES]\n";
print CFG $output_vars;
print CFG "\n";

print CFG "###############################################\n";
print CFG "# Environment specification\n";
print CFG "###############################################\n";
print CFG "[ENV_INITIAL]\n";
print CFG $env_initial;
print CFG "\n";
print CFG "[ENV_TRANSITIONS]\n";
print CFG $env_transitions;
print CFG "\n";
print CFG "[ENV_FAIRNESS]\n";
print CFG $env_fairness;
print CFG "\n";

print CFG "###############################################\n";
print CFG "# System specification\n";
print CFG "###############################################\n";
print CFG "[SYS_INITIAL]\n";
print CFG $sys_initial;
print CFG "\n";
print CFG "[SYS_TRANSITIONS]\n";
print CFG $sys_transitions;
print CFG "\n";
print CFG "[SYS_FAIRNESS]\n";
print CFG $sys_fairness;
print CFG "\n";
close CFG;


###############################################
# PRINT CONFIG FILE
###############################################
print "Generating DBW file $prefix-dbw.v\n";
open (DBW, ">$prefix-dbw.v");

# DBW for guarantee 7
print DBW "//Note that the DBW for G7 works only for two receivers.\n";
print DBW "module DBW7(stateG7_1_n, stateG7_0_n, stateG7_1_p, stateG7_0_p, BtoR_REQ0_p, BtoR_REQ1_p);\n";
print DBW "\tinput  stateG7_1_p, stateG7_0_p, BtoR_REQ0_p, BtoR_REQ1_p;\n";
print DBW "\toutput stateG7_1_n, stateG7_0_n;\n";
print DBW "\twire    stateG7_1_p, stateG7_0_p, BtoR_REQ0_p, BtoR_REQ1_p;\n";
print DBW "\twire    stateG7_1_n, stateG7_0_n;\n\n";

print DBW "\tassign  stateG7_1_n = (!stateG7_1_p && !BtoR_REQ0_p &&  BtoR_REQ1_p)||\n";
print DBW "\t                      ( stateG7_1_p && !BtoR_REQ0_p && !BtoR_REQ1_p)||\n";
print DBW "\t                      ( stateG7_1_p && !stateG7_0_p && !BtoR_REQ0_p && BtoR_REQ1_p);\n";

print DBW "\tassign  stateG7_0_n = (!stateG7_1_p && !BtoR_REQ0_p && !BtoR_REQ1_p);\n";

print DBW "endmodule\n";

# DBW for guarantee 12
print DBW "module DBW12(stateG12_n, stateG12_p, EMPTY_p, DEQ_p);\n";
print DBW "\tinput  stateG12_p, EMPTY_p, DEQ_p;\n";
print DBW "\toutput stateG12_n;\n";
print DBW "\twire    stateG12_n, stateG12_p, EMPTY_p, DEQ_p;\n\n";

print DBW "\tassign  stateG12_n = (!stateG12_p && !DEQ_p && !EMPTY_p)||\n";
print DBW "\t                     ( stateG12_p && !DEQ_p);\n";

print DBW "endmodule\n";

close DBW;
