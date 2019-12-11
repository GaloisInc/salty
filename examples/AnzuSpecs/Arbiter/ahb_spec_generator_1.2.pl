#! /usr/bin/perl -w 

###############################################
# perl script to generated a config file for
# synthesizing an ARM AMBA AHB with an arbitrary
# number of masters.
#
# Usage: ./ahb_spec_generator.pl <num_of_masters>
#
###############################################
#use strict;
use POSIX; # qw(ceil floor);

###############################################
sub buildStateString {
    my ($state_name, $op, $num_states, $value, $padd_value, $add_next) = @_;
    my $result = "";

    if (! defined $add_next) {
    	$add_next = "";
    }
    if(! defined $padd_value) {
	$padd_value = "0";
    }
    
    my $bin = reverse sprintf("%b", $value);
    
    for(my $j = 0; $j < $num_states; $j++) {
	if(!($result eq "")) {
	    $result .= $op;
	}
	
	my $bin_val = $padd_value;
	if($j < length($bin)) {
	    $bin_val = substr($bin, $j, 1);
	}

        $result .= "$add_next(" . $state_name . $j . "=" . $bin_val . ")";
    }

    return $result;
}

###############################################
sub buildHMasterString {
    my ($master_bits, $value) = @_;
    return buildStateString("hmaster", "$and", $master_bits, $value);
}

###############################################
sub buildNegHMasterString {
    my ($master_bits, $value) = @_;
    
    my $bin = sprintf("%b", $value);
    my $new_val = "";
    for(my $j = 0; $j < length($bin); $j++) {
	if(substr($bin, $j, 1) eq "0") {
	    $new_val .= "1";
	} else {
	    $new_val .= "0";
	}
    }
    
    $bin = reverse $new_val;
    my $result = "";
    for(my $j = 0; $j < $master_bits; $j++) {
	if(!($result eq "")) {
	    $result .= "$or";
	}
	
	my $bin_val = "1";
	if($j < length($bin)) {
	    $bin_val = substr($bin, $j, 1);
	}
	
	$result .= "(hmaster" . $j . "=" . $bin_val . ")";
    }
    
    return $result;
}


###############################################
#                MAIN
###############################################

if(! defined($ARGV[0])) {
    print "Usage: ./ahb_spec_generator.pl <num_of_masters> [tlv/low] \n";
    print "        num_of_masters... number of clients the bus can handle\n";
    print "        tlv.............. output config file in tlv-format\n";
    print "        low...............indicates if the signals decide,start, hgrant0 are low sensitive\n";
    exit;
}
my $num_masters = $ARGV[0];

$low = "";
$tlv = 0;
$prefix = "amba";
if (defined $ARGV[1]) {
    $_ = $ARGV[1];
    if (/tlv/) {
	$tlv = 1;
	$prefix = $ARGV[2];
	print "Generated TLV files:\n";
	print "\t$prefix$num_masters.smv, $prefix$num_masters.pf\n";

    } elsif (/low/) {
	print "#Signals decide, start, and hgrant0 are low sensitive\n";
	$low = 1;
    }
}

if ($tlv) {
    $G="[]";
    $F="<>";
    $and=" & ";
    $or=" | ";
    $nFormula=" & ";
    $X="next";
} else {
    $G="G";
    $F="F";
    $and=" * ";
    $or=" + ";
    $nFormula=";";
    $X="X";
}

my $master_bits = ceil((log $num_masters)/(log 2));
if($master_bits == 0) {
    $master_bits = 1;
}
my $master_bits_plus_one = ceil((log($num_masters+1))/(log 2));
if($master_bits == 0) {
    $master_bits = 1;
}

my $env_initial = "";
my $sys_initial = "";
my $env_transitions = "";
my $sys_transitions = "";
my $env_fairness = "";
my $sys_fairness = "";
my $input_vars = "";
my $output_vars = "";

my $var_def = "MODULE main\n \tVAR\n";

###############################################
# ENV_INITIAL and INPUT_VARIABLES
###############################################

$env_initial .= "hready=0$nFormula\n";
$input_vars  .= "hready$nFormula\n";
$var_def .= "\thready: boolean;\n";

for(my $i = 0; $i < $num_masters; $i++) {
    $env_initial .= "hbusreq" . $i . "=0$nFormula\n";
    $env_initial .= "hlock" . $i . "=0$nFormula\n";
    $input_vars .= "hbusreq" . $i . "$nFormula\n";
    $input_vars .= "hlock" . $i . "$nFormula\n";
    $var_def .= "\thbusreq$i: boolean;\n";
    $var_def .= "\thlock$i: boolean;\n";
}

$env_initial .= "hburst0=0$nFormula\n";
$input_vars .= "hburst0$nFormula\n";
$env_initial .= "hburst1=0$nFormula\n";
$input_vars .= "hburst1$nFormula\n";
$var_def .= "\thburst0: boolean;\n";
$var_def .= "\thburst1: boolean;\n";

###############################################
# ENV_TRANSITION
###############################################
for(my $i = 0; $i < $num_masters; $i++) {
#    $env_transitions .= "#Assumption 3:\n";
    $env_transitions .= "$G( hlock$i=1 -> hbusreq$i=1 )$nFormula\n";
}

###############################################
# ENV_FAIRNESS
###############################################
#$env_fairness .= "#Assumption 1: \n";
$env_fairness .= "$G($F(stateA1_1=0))$nFormula\n";
#$env_fairness .= "\n#Assumption 2:\n";
$env_fairness .= "$G($F(hready=1))$nFormula\n";

###############################################
# SYS_INITIAL + OUTPUT_VARIABLES
###############################################

for(my $i = 0; $i < $master_bits; $i++) {
    $sys_initial .= "hmaster" . $i . "=0$nFormula\n";
    $output_vars .= "hmaster" . $i . "$nFormula\n";
    $var_def .= "\thmaster" . $i . ": boolean;\n";
}

$sys_initial .= "hmastlock=0$nFormula\n";
$output_vars .= "hmastlock$nFormula\n";
$var_def .= "\thmastlock: boolean;\n";
if ($low) {
    $sys_initial .= "nstart=0$nFormula\n";
    $output_vars .= "nstart$nFormula\n";
    $var_def .= "\tnstart: boolean;\n";
    $sys_initial .= "ndecide=0$nFormula\n";
    $output_vars .= "ndecide$nFormula\n";
    $var_def .= "\tndecide: boolean;\n";
} else {
    $sys_initial .= "start=1$nFormula\n";
    $output_vars .= "start$nFormula\n";
    $var_def .= "\tstart: boolean;\n";
    $sys_initial .= "decide=1$nFormula\n";
    $output_vars .= "decide$nFormula\n";
    $var_def .= "\tdecide: boolean;\n";
}
$output_vars .= "locked$nFormula\n";
$var_def .= "\tlocked: boolean;\n";
$sys_initial .= "locked=0$nFormula\n";

if ($low) {
    $sys_initial .= "nhgrant0=0$nFormula\n";
    $output_vars .= "nhgrant0$nFormula\n";
    $var_def .= "\tnhgrant0: boolean;\n";
} else {
    $sys_initial .= "hgrant0=1$nFormula\n";
    $output_vars .= "hgrant0$nFormula\n";
    $var_def .= "\thgrant0: boolean;\n";
}
for(my $i = 1; $i < $num_masters; $i++) {
    $sys_initial .= "hgrant".$i."=0$nFormula\n";
    $output_vars .= "hgrant".$i."$nFormula\n";
    $var_def .= "\thgrant$i: boolean;\n";
}

#busreq = hbusreq[hmaster]
$sys_initial .= "busreq=0$nFormula\n";
$output_vars .= "busreq$nFormula\n";
$var_def .= "\tbusreq: boolean;\n";

#Assumption 1:
$sys_initial .= "stateA1_0=0$nFormula\n";
$output_vars .= "stateA1_0$nFormula\n";
$var_def     .= "\tstateA1_0: boolean;\n";
$sys_initial .= "stateA1_1=0$nFormula\n";
$output_vars .= "stateA1_1$nFormula\n";
$var_def     .= "\tstateA1_1: boolean;\n";


#Guarantee 2:
$sys_initial .= "stateG2=0$nFormula\n";
$output_vars .= "stateG2$nFormula\n";
$var_def .= "\tstateG2: boolean;\n";

#Guarantee 3:
$sys_initial .= "stateG3_0=0$nFormula\n";
$output_vars .= "stateG3_0$nFormula\n";
$var_def .= "\tstateG3_0: boolean;\n";
$sys_initial .= "stateG3_1=0$nFormula\n";
$output_vars .= "stateG3_1$nFormula\n";
$var_def .= "\tstateG3_1: boolean;\n";
$sys_initial .= "stateG3_2=0$nFormula\n";
$output_vars .= "stateG3_2$nFormula\n";
$var_def .= "\tstateG3_2: boolean;\n";

#Guarantee 10:
for(my $i = 1; $i < $num_masters; $i++) {
    $sys_initial .= "stateG10_" . $i . "=0$nFormula\n";
    $output_vars .= "stateG10_" . $i . "$nFormula\n";
    $var_def .= "\tstateG10_" . $i . ": boolean;\n";
}

###############################################
# SYS_TRANSITION
###############################################

# busreq = hbusreq[hmaster]
for(my $i = 0; $i < $num_masters; $i++) {
    my $hmaster = buildHMasterString($master_bits, $i);
    my $hmaster_X = buildStateString("hmaster", "$and", $master_bits, $i, 0, $X);
    $sys_transitions .= "$G($hmaster -> (hbusreq$i=0 <-> busreq=0))$nFormula\n";
}

#Assumption 1:
#$sys_transitions .= "#Assumption 1:\n";
#state 00
$sys_transitions .= "$G(((stateA1_1=0)$and(stateA1_0=0)$and";
$sys_transitions .= "((hmastlock=0)$or(hburst0=1)$or(hburst1=1))) ->\n";
$sys_transitions .= " $X((stateA1_1=0)$and(stateA1_0=0)))$nFormula\n";
$sys_transitions .= "$G(((stateA1_1=0)$and(stateA1_0=0)$and";
$sys_transitions .= " (hmastlock=1)$and(hburst0=0)$and(hburst1=0)) ->\n";
$sys_transitions .= " $X((stateA1_1=1)$and(stateA1_0=0)))$nFormula\n";

#state 10
$sys_transitions .= "$G(((stateA1_1=1)$and(stateA1_0=0)$and(busreq=1)) ->\n";
$sys_transitions .= " $X((stateA1_1=1)$and(stateA1_0=0)))$nFormula\n";
$sys_transitions .= "$G(((stateA1_1=1)$and(stateA1_0=0)$and(busreq=0)$and";
$sys_transitions .= "((hmastlock=0)$or(hburst0=1)$or(hburst1=1))) ->\n";
$sys_transitions .= " $X((stateA1_1=0)$and(stateA1_0=0)))$nFormula\n";
$sys_transitions .= "$G(((stateA1_1=1)$and(stateA1_0=0)$and(busreq=0)$and";
$sys_transitions .= " (hmastlock=1)$and(hburst0=0)$and(hburst1=0)) ->\n";
$sys_transitions .= " $X((stateA1_1=0)$and(stateA1_0=1)))$nFormula\n";

#state 01
$sys_transitions .= "$G(((stateA1_1=0)$and(stateA1_0=1)$and(busreq=1)) ->\n";
$sys_transitions .= " $X((stateA1_1=1)$and(stateA1_0=0)))$nFormula\n";
$sys_transitions .= "$G(((stateA1_1=0)$and(stateA1_0=1)$and";
$sys_transitions .= " (hmastlock=1)$and(hburst0=0)$and(hburst1=0)) ->\n";
$sys_transitions .= " $X((stateA1_1=1)$and(stateA1_0=0)))$nFormula\n";
$sys_transitions .= "$G(((stateA1_1=0)$and(stateA1_0=1)$and(busreq=0)$and";
$sys_transitions .= "((hmastlock=0)$or(hburst0=1)$or(hburst1=1))) ->\n";
$sys_transitions .= " $X((stateA1_1=0)$and(stateA1_0=0)))$nFormula\n";

#Guarantee 1:
#$sys_transitions .= "\n#Guarantee 1:\n";
if ($low) {
    $sys_transitions .= "$G((hready=0) -> $X(nstart=1))$nFormula\n";
} else {
    $sys_transitions .= "$G((hready=0) -> $X(start=0))$nFormula\n";
}

#Guarantee 2:
#$sys_transitions .= "\n#Guarantee 2:\n";
$sys_transitions .= "$G(((stateG2=0)$and";
if ($low) {
    $sys_transitions .= "((hmastlock=0)$or(nstart=1)$or";
} else {
    $sys_transitions .= "((hmastlock=0)$or(start=0)$or";
}
$sys_transitions .= "(hburst0=1)$or(hburst1=1))) -> ";
$sys_transitions .= "$X(stateG2=0))$nFormula\n";

$sys_transitions .= "$G(((stateG2=0)$and";
if ($low) {
    $sys_transitions .= " (hmastlock=1)$and(nstart=0)$and";
} else {
    $sys_transitions .= " (hmastlock=1)$and(start=1)$and";
}
$sys_transitions .= "(hburst0=0)$and(hburst1=0))  -> ";
$sys_transitions .= "$X(stateG2=1))$nFormula\n";

if ($low) {
    $sys_transitions .= "$G(((stateG2=1)$and(nstart=1)$and(busreq=1)) -> ";
    $sys_transitions .= "$X(stateG2=1))$nFormula\n";
    $sys_transitions .= "$G(((stateG2=1)$and(nstart=0)) -> FALSE)$nFormula\n";
    $sys_transitions .= "$G(((stateG2=1)$and(nstart=1)$and(busreq=0)) -> ";
    $sys_transitions .= "$X(stateG2=0))$nFormula\n";
} else {
    $sys_transitions .= "$G(((stateG2=1)$and(start=0)$and(busreq=1)) -> ";
    $sys_transitions .= "$X(stateG2=1))$nFormula\n";
    $sys_transitions .= "$G(((stateG2=1)$and(start=1)) -> FALSE)$nFormula\n";
    $sys_transitions .= "$G(((stateG2=1)$and(start=0)$and(busreq=0)) -> ";
    $sys_transitions .= "$X(stateG2=0))$nFormula\n";
}

#Guarantee 3:
#$sys_transitions .= "\n#Guarantee 3:\n";
if ($low) {
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=0)$and(stateG3_2=0)$and\n";
    $sys_transitions .= "  ((hmastlock=0)$or(nstart=1)$or((hburst0=1)$or(hburst1=0)))) ->\n";
    $sys_transitions .= "  ($X(stateG3_0=0)$and$X(stateG3_1=0)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=0)$and(stateG3_2=0)$and\n";
    $sys_transitions .= "  ((hmastlock=1)$and(nstart=0)$and((hburst0=0)$and(hburst1=1))$and(hready=0))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=1)$and$X(stateG3_1=0)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=0)$and(stateG3_2=0)$and\n";
    $sys_transitions .= "  ((hmastlock=1)$and(nstart=0)$and((hburst0=0)$and(hburst1=1))$and(hready=1))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=0)$and$X(stateG3_1=1)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= " \n";
    $sys_transitions .= "$G(((stateG3_0=1)$and(stateG3_1=0)$and(stateG3_2=0)$and((nstart=1)$and(hready=0))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=1)$and$X(stateG3_1=0)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= "$G(((stateG3_0=1)$and(stateG3_1=0)$and(stateG3_2=0)$and((nstart=1)$and(hready=1))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=0)$and$X(stateG3_1=1)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= "\n";
    $sys_transitions .= "$G(((stateG3_0=1)$and(stateG3_1=0)$and(stateG3_2=0)$and((nstart=0))) -> FALSE); \n";
    $sys_transitions .= "\n";
    $sys_transitions .= " \n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=1)$and(stateG3_2=0)$and((nstart=1)$and(hready=0))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=0)$and$X(stateG3_1=1)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=1)$and(stateG3_2=0)$and((nstart=1)$and(hready=1))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=1)$and$X(stateG3_1=1)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=1)$and(stateG3_2=0)$and((nstart=0))) -> FALSE); \n";
    $sys_transitions .= " \n";
    $sys_transitions .= "$G(((stateG3_0=1)$and(stateG3_1=1)$and(stateG3_2=0)$and((nstart=1)$and(hready=0))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=1)$and$X(stateG3_1=1)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= "$G(((stateG3_0=1)$and(stateG3_1=1)$and(stateG3_2=0)$and((nstart=1)$and(hready=1))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=0)$and$X(stateG3_1=0)$and$X(stateG3_2=1))); \n";
    $sys_transitions .= "$G(((stateG3_0=1)$and(stateG3_1=1)$and(stateG3_2=0)$and((nstart=0))) -> FALSE); \n";
    $sys_transitions .= " \n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=0)$and(stateG3_2=1)$and((nstart=1)$and(hready=0))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=0)$and$X(stateG3_1=0)$and$X(stateG3_2=1))); \n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=0)$and(stateG3_2=1)$and((nstart=1)$and(hready=1))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=0)$and$X(stateG3_1=0)$and$X(stateG3_2=0)))$nFormula\n";
    $sys_transitions .= "\n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=0)$and(stateG3_2=1)$and((nstart=0))) -> FALSE); \n";
} else {
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=0)$and(stateG3_2=0)$and\n";
    $sys_transitions .= "  ((hmastlock=0)$or(start=0)$or((hburst0=1)$or(hburst1=0)))) ->\n";
    $sys_transitions .= "  ($X(stateG3_0=0)$and$X(stateG3_1=0)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=0)$and(stateG3_2=0)$and\n";
    $sys_transitions .= "  ((hmastlock=1)$and(start=1)$and((hburst0=0)$and(hburst1=1))$and(hready=0))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=1)$and$X(stateG3_1=0)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=0)$and(stateG3_2=0)$and\n";
    $sys_transitions .= "  ((hmastlock=1)$and(start=1)$and((hburst0=0)$and(hburst1=1))$and(hready=1))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=0)$and$X(stateG3_1=1)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= " \n";
    $sys_transitions .= "$G(((stateG3_0=1)$and(stateG3_1=0)$and(stateG3_2=0)$and((start=0)$and(hready=0))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=1)$and$X(stateG3_1=0)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= "$G(((stateG3_0=1)$and(stateG3_1=0)$and(stateG3_2=0)$and((start=0)$and(hready=1))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=0)$and$X(stateG3_1=1)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= "\n";
    $sys_transitions .= "$G(((stateG3_0=1)$and(stateG3_1=0)$and(stateG3_2=0)$and((start=1))) -> FALSE); \n";
    $sys_transitions .= "\n";
    $sys_transitions .= " \n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=1)$and(stateG3_2=0)$and((start=0)$and(hready=0))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=0)$and$X(stateG3_1=1)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=1)$and(stateG3_2=0)$and((start=0)$and(hready=1))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=1)$and$X(stateG3_1=1)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=1)$and(stateG3_2=0)$and((start=1))) -> FALSE); \n";
    $sys_transitions .= " \n";
    $sys_transitions .= "$G(((stateG3_0=1)$and(stateG3_1=1)$and(stateG3_2=0)$and((start=0)$and(hready=0))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=1)$and$X(stateG3_1=1)$and$X(stateG3_2=0))); \n";
    $sys_transitions .= "$G(((stateG3_0=1)$and(stateG3_1=1)$and(stateG3_2=0)$and((start=0)$and(hready=1))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=0)$and$X(stateG3_1=0)$and$X(stateG3_2=1))); \n";
    $sys_transitions .= "$G(((stateG3_0=1)$and(stateG3_1=1)$and(stateG3_2=0)$and((start=1))) -> FALSE); \n";
    $sys_transitions .= " \n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=0)$and(stateG3_2=1)$and((start=0)$and(hready=0))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=0)$and$X(stateG3_1=0)$and$X(stateG3_2=1))); \n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=0)$and(stateG3_2=1)$and((start=0)$and(hready=1))) -> \n";
    $sys_transitions .= "   ($X(stateG3_0=0)$and$X(stateG3_1=0)$and$X(stateG3_2=0)))$nFormula\n";
    $sys_transitions .= "\n";
    $sys_transitions .= "$G(((stateG3_0=0)$and(stateG3_1=0)$and(stateG3_2=1)$and((start=1))) -> FALSE); \n";
}

#Guarantee 4 and 5:
#$sys_transitions .= "\n#Guarantee 4 and 5:\n";
for(my $i = 0; $i < $num_masters; $i++) {
  my $hmaster_X = buildStateString("hmaster", "$and", $master_bits, $i, 0, $X);
#  $sys_transitions .= "#  Master ".$i.":\n";
  if ($low && ($i == 0)) {
      $sys_transitions .= "$G((hready=1) -> ((nhgrant" . $i . "=0) <-> (" . $hmaster_X . ")))$nFormula\n";
  } else {
      $sys_transitions .= "$G((hready=1) -> ((hgrant" . $i . "=1) <-> (" . $hmaster_X . ")))$nFormula\n";
  }
}
#$sys_transitions .= "#  HMASTLOCK:\n";
$sys_transitions .= "$G((hready=1) -> (locked=0 <-> $X(hmastlock=0)))$nFormula\n";

#Guarantee 6.1:
#FIXME: I would be sufficient to have one formula for each bit of hmaster
#$sys_transitions .= "\n#Guarantee 6.1:\n";
for(my $i = 0; $i < $num_masters; $i++) {
  my $hmaster = buildHMasterString($master_bits, $i);
  my $hmaster_X = buildStateString("hmaster", "$and", $master_bits, $i, 0, $X);
#  $sys_transitions .= "#  Master ".$i.":\n";
  if ($low) {
      $sys_transitions .= "$G($X(nstart=1) -> ((" . $hmaster . ") <-> (" . $hmaster_X . ")))$nFormula\n";
  } else {
      $sys_transitions .= "$G($X(start=0) -> ((" . $hmaster . ") <-> (" . $hmaster_X . ")))$nFormula\n";
  }
}

#Guarantee 6.2:
#$sys_transitions .= "\n#Guarantee 6.2:\n";
if ($low) {
    $sys_transitions .= "$G((($X(nstart=1))) -> ((hmastlock=1) <-> $X(hmastlock=1)))$nFormula\n";
} else {
    $sys_transitions .= "$G((($X(start=0))) -> ((hmastlock=1) <-> $X(hmastlock=1)))$nFormula\n";
}

#Guarantee 7:
#FIXME: formula can be written as
# G((decide=1 $and $X(hgrant$i=1))-> (hlock$i=1 <-> $X(locked=1)))
#$sys_transitions .= "\n#Guarantee 7:\n";
my $norequest = "";
for(my $i = 0; $i < $num_masters; $i++) {
    if ($low) {
	if ($i == 0) {
	    $sys_transitions .= "$G((ndecide=0 $and hlock$i=1 $and $X(nhgrant$i=0))->$X(locked=1))$nFormula\n";
	    $sys_transitions .= "$G((ndecide=0 $and hlock$i=0 $and $X(nhgrant$i=0))->$X(locked=0))$nFormula\n";
	} else {
	    $sys_transitions .= "$G((ndecide=0 $and hlock$i=1 $and $X(hgrant$i=1))->$X(locked=1))$nFormula\n";
	    $sys_transitions .= "$G((ndecide=0 $and hlock$i=0 $and $X(hgrant$i=1))->$X(locked=0))$nFormula\n";
	}
    } else {
	$sys_transitions .= "$G((decide=1 $and hlock$i=1 $and $X(hgrant$i=1))->$X(locked=1))$nFormula\n";
	$sys_transitions .= "$G((decide=1 $and hlock$i=0 $and $X(hgrant$i=1))->$X(locked=0))$nFormula\n";

    }
    $norequest .= "hbusreq$i=0";
    $norequest .= " $and " if ($i < $num_masters-1);
}

#Guarantee 8:
#MW: Note, that this formula changes with respect to the number of grant signals
#$sys_transitions .= "\n#Guarantee 8:\n";
my $tmp_g8 = "";
for(my $i = 0; $i < $num_masters; $i++) {
    if ($low) {
	if ($i == 0) {
	    $sys_transitions .= "$G((ndecide=1)->(((nhgrant" . $i . "=1)<->$X(nhgrant" . $i . "=1))))$nFormula\n";
	} else {
	    $sys_transitions .= "$G((ndecide=1)->(((hgrant" . $i . "=0)<->$X(hgrant" . $i . "=0))))$nFormula\n";
	}
    } else {
	$sys_transitions .= "$G((decide=0)->(((hgrant" . $i . "=0)<->$X(hgrant" . $i . "=0))))$nFormula\n";

    }
}
if ($low) {
    $sys_transitions .= "$G((ndecide=1)->(locked=0 <-> $X(locked=0)))$nFormula\n";
} else {
    $sys_transitions .= "$G((decide=0)->(locked=0 <-> $X(locked=0)))$nFormula\n";
}

#Guarantee 10:
#$sys_transitions .= "\n#Guarantee 10:\n";
for(my $i = 1; $i < $num_masters; $i++) {
  my $neghmaster = buildNegHMasterString($master_bits, $i);
  my $hmaster = buildHMasterString($master_bits, $i);

#  $sys_transitions .= "#  Master ".$i.":\n";
  if ($low && ($i == 0)) {
      $sys_transitions .= "$G(((stateG10_".$i."=0)$and(((nhgrant".$i."=0)$or(hbusreq".$i."=1))))->$X(stateG10_".$i."=0))$nFormula\n";
      $sys_transitions .= "$G(((stateG10_".$i."=0)$and((nhgrant".$i."=1)$and(hbusreq".$i."=0)))->$X(stateG10_".$i."=1))$nFormula\n";
      $sys_transitions .= "$G(((stateG10_".$i."=1)$and((nhgrant".$i."=1)$and(hbusreq".$i."=0)))->$X(stateG10_".$i."=1))$nFormula\n";
      $sys_transitions .= "$G(((stateG10_".$i."=1)$and(((nhgrant".$i."=0))$and(hbusreq".$i."=0)))->FALSE)$nFormula\n";
      $sys_transitions .= "$G(((stateG10_".$i."=1)$and(hbusreq".$i."=1))->$X(stateG10_".$i."=0))$nFormula\n";
  } else {
      $sys_transitions .= "$G(((stateG10_".$i."=0)$and(((hgrant".$i."=1)$or(hbusreq".$i."=1))))->$X(stateG10_".$i."=0))$nFormula\n";
      $sys_transitions .= "$G(((stateG10_".$i."=0)$and((hgrant".$i."=0)$and(hbusreq".$i."=0)))->$X(stateG10_".$i."=1))$nFormula\n";
      $sys_transitions .= "$G(((stateG10_".$i."=1)$and((hgrant".$i."=0)$and(hbusreq".$i."=0)))->$X(stateG10_".$i."=1))$nFormula\n";
      $sys_transitions .= "$G(((stateG10_".$i."=1)$and(((hgrant".$i."=1))$and(hbusreq".$i."=0)))->FALSE)$nFormula\n";
      $sys_transitions .= "$G(((stateG10_".$i."=1)$and(hbusreq".$i."=1))->$X(stateG10_".$i."=0))$nFormula\n";
  }
}
#$sys_transitions .= "#default master\n";
if ($low) {
    $sys_transitions .= "$G((ndecide=0 $and ".$norequest.") -> $X(nhgrant0=0))$nFormula\n";
} else {
    $sys_transitions .= "$G((decide=1 $and ".$norequest.") -> $X(hgrant0=1))$nFormula\n";
}

###############################################
# SYS_FAIRNESS
###############################################
#Guarantee 2:
#$sys_fairness .= "\n#Guarantee 2:\n";
$sys_fairness .= "$G($F(stateG2=0))$nFormula\n";

#Guarantee 3:
#$sys_fairness .= "\n#Guarantee 3:\n";
$sys_fairness .= "$G($F((stateG3_0=0) $and (stateG3_1=0) $and (stateG3_2=0)))$nFormula\n";

#Guarantee 9:
#$sys_fairness .= "\n#Guarantee 9:\n";
for(my $i = 0; $i < $num_masters; $i++) {
  $sys_fairness .= "$G($F((" . buildHMasterString($master_bits, $i) . ") $or hbusreq" . $i . "=0))$nFormula\n";
}

###############################################
# PRINT CONFIG FILE
###############################################

if (not $tlv) {	
    print "###############################################\n";
    print "# Input variable definition\n";
    print "###############################################\n";
    print "[INPUT_VARIABLES]\n";
    print $input_vars;
    print "\n";
    
    print "###############################################\n";
    print "# Output variable definition\n";
    print "###############################################\n";
    print "[OUTPUT_VARIABLES]\n";
    print $output_vars;
    print "\n";
    
    print "###############################################\n";
    print "# Environment specification\n";
    print "###############################################\n";
    print "[ENV_INITIAL]\n";
    print $env_initial;
    print "\n";
    print "[ENV_TRANSITIONS]\n";
    print $env_transitions;
    print "\n";
    print "[ENV_FAIRNESS]\n";
    print $env_fairness;
    print "\n";
    
    
    print "###############################################\n";
    print "# System specification\n";
    print "###############################################\n";
    print "[SYS_INITIAL]\n";
    print $sys_initial;
    print "\n";
    print "[SYS_TRANSITIONS]\n";
    print $sys_transitions;
    print "\n";
    print "[SYS_FAIRNESS]\n";
    print $sys_fairness;
    print "\n";
} else {
    open (SMV, ">$prefix$num_masters.smv");
    print SMV $var_def;
    close SMV;
    
    $input_vars =~ s/ & $//;
    $output_vars =~ s/ & $//; 
    $env_fairness =~ s/ & $//; 
    open (PF, ">$prefix$num_masters.pf");
    
    print PF "Load \"synthesis.tlv\";\n";
    print PF "Let ts := compile_spec(\n";
    print PF "\tltl(\n";
    print PF "(\n";
    print PF $env_initial;
    print PF $env_transitions;
    print PF $env_fairness;
    print PF ") -> (\n";
    print PF $sys_initial;
    print PF $sys_transitions;
    print PF $sys_fairness;
    print PF ")\n";
    print PF "),\n";
    print PF $input_vars;
    print PF ",\n";
    print PF $output_vars;
    print PF ");\n";
    
    close PF;
}
