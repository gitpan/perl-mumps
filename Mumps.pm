# PerlMUMPS by Ariel Brosh
# Usage is free, including commercial use, enterprise and legacy use
# However, any modifications should be notified to the author
# Email: mumps@atheist.org.il


package Language::Mumps;
$VERSION = 1.05;
use Fcntl;
use strict vars;
use vars qw($FETCH $STORE $DB $SER $IMPORT @TYING $xpos $ypos
  %symbols $selected_io $flag @handlers @xreg @yreg
  $curses_inside $varstack %RES $RESKEYS %COMMANDS $scope_do
  %FUNCTIONS %FUNS @tmpvars $tmphash $infun $scopes @stack
  @program %bookmarks $lnum $forgiveful $forscope %dbs
  $VERSION);

%COMMANDS = qw(B BREAK C CLOSE D DO E ELSE F FOR G GOTO HALT HALT
               H HANG I IF J JOB K KILL L LOCK O OPEN Q QUIT
               R READ S SET U USE V VIEW W WRITE X XECUTE
               ZE HALT ZP ZP ZFUNCTION ZFUNCTION
               ZRETURN ZRETURN ZD ZD);

%FUNCTIONS = qw(I IO T TEST P PIECE H HOROLOG J JOB 
                 X X Y Y ZDATE ZD ZA ZN);

%FUNS = (
         'ASCII' => [{'lval' => 0, 'prot' => 'II'},
                     {'lval' => 0, 'prot' => 'I'}],
         'CHAR' => [{'lval' => 0, 'prot' => 'L'}],
         'DATA' => [{'lval' => 0, 'prot' => 'O'}],
         'EXTRACT' => [{'lval' => 0, 'prot' => 'I'},
                       {'lval' => 0, 'prot' => 'II'},
                       {'lval' => 0, 'prot' => 'III'}],
         'FIND' => [{'lval' => 0, 'prot' => 'II'},
                     {'lval' => 0, 'prot' => 'III'}],
         'JOB' => [{'lval' => 0, 'prot' => ''}],
         'JUSTIFY' => [{'lval' => 0, 'prot' => 'II'},
                     {'lval' => 0, 'prot' => 'III'}],
         'HOROLOG' => [{'lval' => 0, 'prot' => ''}],
         'IO' => [{'lval' => 1, 'prot' => ''}],
         'LEN' => [{'lval' => 0, 'prot' => 'II'},
                     {'lval' => 0, 'prot' => 'I'}],
         'NEXT' => [{'lval' => 0, 'prot' => 'O'}],
         'ORDER' => [{'lval' => 0, 'prot' => 'O'}],
         'PIECE' => [{'lval' => 1, 'prot' => 'OII'},
                 {'lval' => 0, 'prot' => 'III'},
                 {'lval' => 0, 'prot' => 'IIII'}],
         'RANDOM' => [{'lval' => 0, 'prot' => 'I'}],
         'SELECT' => [{'lval' => 0, 'prot' => 'T'}],
         'TEST' => [{'lval' => 1, 'prot' => ''}],
         'X' => [{'lval' => 0, 'prot' => ''}],
         'Y' => [{'lval' => 0, 'prot' => ''}],
         'ZAB' => [{'lval' => 0, 'prot' => 'I'}],
         'ZB' => [{'lval' => 0, 'prot' => 'I'}],
         'ZCD' => [{'lval' => 0, 'prot' => ''},
                   {'lval' => 0, 'prot' => 'I'}],
         'ZCL' => [{'lval' => 0, 'prot' => ''},
                   {'lval' => 0, 'prot' => 'I'}],
         'ZD' => [{'lval' => 0, 'prot' => ''}],
         'ZD1' => [{'lval' => 0, 'prot' => ''}],
         'ZD2' => [{'lval' => 0, 'prot' => 'I'}],
         'ZD3' => [{'lval' => 0, 'prot' => 'III'}],
         'ZD4' => [{'lval' => 0, 'prot' => 'III'}],
         'ZD5' => [{'lval' => 0, 'prot' => 'III'}],
         'ZD6' => [{'lval' => 0, 'prot' => 'I'},
                  {'lval' => 0, 'prot' => ''}],
         'ZD7' => [{'lval' => 0, 'prot' => 'I'},
                  {'lval' => 0, 'prot' => ''}],
         'ZD8' => [{'lval' => 0, 'prot' => 'I'},
                  {'lval' => 0, 'prot' => ''}],
         'ZD9' => [{'lval' => 0, 'prot' => 'I'},
                  {'lval' => 0, 'prot' => ''}],
         'ZDBI' => [{'lval' => 0, 'prot' => 'IIIIO'}],
         'ZF' => [{'lval' => 0, 'prot' => 'I'}],
         'ZH' => [{'lval' => 0, 'prot' => 'I'}],
         'ZL' => [{'lval' => 0, 'prot' => 'II'},
                  {'lval' => 0, 'prot' => 'I'}],
         'ZN' => [{'lval' => 0, 'prot' => 'I'}],
         'ZR' => [{'lval' => 0, 'prot' => 'I'}],
         'ZS' => [{'lval' => 0, 'prot' => 'I'}],
         'ZSQR' => [{'lval' => 0, 'prot' => 'I'}],
         'ZT' => [{'lval' => 0, 'prot' => 'I'}],
         'ZVARIABLE' => [{'lval' => 0, 'prot' => 'I'}],
         );

sub m2pl {
    my $line = shift;

    $line =~ s/^(\w+) {8}/$1\t/ if ($forgiveful);
    if ($line =~ s/^\%//) {
        return "$line\n";
    }

    if ($line =~ s/^\#//) {
        return "";
    }

    unless ($line =~ /\t/) {
        return "Language::Mumps::write('$line');\n";
    }

    &resetvars;

    my ($label, $llin) = split(/\t+/, $line, 2);
    $line = $llin;
    die "Illegal label $label" unless (!$label || $label =~ /^[a-z]\w*/i);
    $bookmarks{$label} = $lnum;
    $label = "__lbl_Mumps_$label\: " if ($label);
    $label . &ml2pl($line);
}

sub ml2pl {
    my $line = shift;
    my ($res, $pre, $post, $tmp, $code);
    while ($line) {
        my ($token, $cond);
        if ($line =~ s/^\s*(\S*?)\s+//) {
            $token = $1;
        } else {
            $token = $line;
            $line = '';
        }

        if ($token eq '}') {
            die "Unexpected right bracket" unless ($scopes--);
            $code .= "}\n";
            next;
        }

        if ($token =~ /^([a-z]\w*):(.*)$/i) {
            $token = $1;
            $cond = $2;
        }

        if ($cond) {
            ($pre, $tmp) = &makecond($cond);
            $pre .= "if ($tmp) {\n";
            $post = "\n}";
        }

        $token = uc($token);

        my ($k, $v);
        foreach (keys %COMMANDS) {
            if ($_ eq $token || $COMMANDS{$_} eq $token) {
                $res = &{$COMMANDS{$_}}($line);
                $line =~ s/^\s*//;
                goto success;
            }
        }
        die "Unrecognized command $token";
success:
        $code .= "$pre$res$post\n";
    }
    $code;
}

sub compile {
    my $text = shift;
    my @lines = split(/\r?\n/, $text);
    %bookmarks =();
    @program = @lines;
    local($scopes);
    $lnum = 0;
    my @code = map {++$lnum; "# $lnum) $_\n" . &m2pl($_);} @lines;
    die "Unclosed brackets" if ($scopes);
    join("", "use Language::Mumps qw(Runtime $IMPORT);\nno strict;\n",  @code,
              "### end\n", &m2pl("\tQUIT"));
}

sub evaluate {
    my $prog = shift;
    my $code = &compile($prog);
    local (@stack);
    $@ = undef;
    eval $code;
    die $@ if ($@);
}

sub interprete {
    my $fn = shift;
    open(I, $fn);
    my $prog = join("", <I>);
    close(I);
    evaluate($prog);
}

sub translate {
    my ($i, $o) = @_;
    open(I, $i);
    my $prog = join("", <I>);
    close(I);
    my $code = &compile($prog);
    open(O, ">$o");
    print O <<EOM;
#############################################################################
# This Perl script was created by the MUMPS to Perl compiler by Ariel Brosh #
#############################################################################

$code

1;
EOM
    close(o);
}

sub list {
    my ($line, $off);
    my $lnum = ($line > 0) ? ($line - 1) : $bookmarks{$line} || die "Unknown label";
    $program[$lnum - 1 + $off];
}

sub BREAK {
    return "exit;";
}

sub CLOSE ($) {
    my ($code, $var) = &makelist($_[0]);
    return $code . <<EOM;
foreach ($var) {
    die "Can't CLOSE unit 5" if (\$_ == 5);
    close($Language::Mumps::handlers[\$_]);
}
EOM
}

sub DO ($) {
    if ($_[0] =~ s/^\s*([a-z]\w*)\b//i) {
        ++$scope_do;
        my $lbl = &nextvar("d$scope_do");
        return <<EOM;
push(\@Language::Mumps::stack, '$lbl');
goto __lbl_Mumps_$1;
$lbl:
EOM
    }
    if ($_[0] =~ /^[\@"]/) {
        my ($code, $var) = &makeexp($_[0]);
        return $code . "Language::Mumps::interprete($var);";
    }
    if ($_[0] =~ /^\$\$/) {
        my ($code, $var) = &makeexp($_[0]);
        return $code . "\$Language::Mumps::flag = $var ? 1 : undef;";
    }
    $_[0] =~ s/\s.*$//;
    die "Illegal argument for DO $_[0]";
}

sub ELSE ($) {
    my $code = "unless (\$Language::Mumps::flag) {";
    if ($_[0] =~ s/^\{\s*//) {
        $scopes++;
        return $code;
    }
    my $block = &ml2pl($_[0]);
    "$code\n$block}";
}

sub FOR ($) {
    unless ($_[0]) {
        die "Iterator expected in FOR";
    }
    my ($itercode, $lvar) = &makevar($_[0]);
    my $var = $lvar->lval;
    my $itervar = &nextvar();
    my $eachlist = &nextvar('@');
    my $f = &nextvar('$');
    my $t = &nextvar('$');
    my $s = &nextvar('$');
    $itercode .= "*$itervar = \\$var;\n";
    $var = "\$$itervar";
    die "= expected in FOR" unless ($_[0] =~ s/^\=//);
    my $procname = "__tmpfor" . ++$forscope;
    my ($flag, $listflag);
    my $first = 1;
    while (1) {
        $flag = 1 unless ($_[0] && $_[0] !~ /^\s/);
        die "Comma expected in FOR" unless ($first || $_[0] =~ s/^,// || $flag);
        $first = undef;
        my ($code, $val) = &makeexp($_[0]);
        if ($flag || $_[0] =~ s/^\://) {
            $itercode .= "foreach \$var ($eachlist) " .
                 "{&$procname;}\n\$eachlist = ();\n" if ($listflag);
            last if ($flag);
            $listflag = undef;
            $itercode .= $code;
            $itercode .= "$f = $val;\n";

            ($code, $val) = &makeexp($_[0]);
            $itercode .= $code;
            $itercode .= "$s = $val;\n";

            if ($_[0] && $_[0] !~ /^\s/) {
                die "Upper bound expected in FOR" unless ($_[0] =~ s/^://);
                my ($code, $val) = &makeexp($_[0]);
                $itercode .= $code;
                $itercode .= "$t = $val;\n";
            } else {
                $itercode .= "$t = $f - $s * 2;\n";
            }
#            my $sign = (qw(< == >))[($f <=> $t) + 1];
#            my $step = (qw(+ + -))[($f <=> $t) + 1];
#            my $cond = ($t ? "$var $sign $t" : 1);
#            my $incr = (abs($s) == 1) ? ($var . ($step x 2))
#                    : "$var $step= " . abs($s);
            my $for = "($var = $f, $t += $s; " .
              "$var != $t && ($var <=> $t) == ($f <=> $t); " .
              "$var += $s)";
            $itercode .= "for $for {\&$procname;}\n";
        } else {
            $itercode .= $code . "push($eachlist, $val);\n";
            $listflag = 1;
        }
    }
    $itercode .= "*$itervar = \\\$sysundef;\n";
    $_[0] =~ s/^\s*//;
    die "Code expected in FOR" unless ($_[0]);
    $itercode .= "sub $procname {\n";
    if ($_[0] =~ s/^\{\s*//) {
        $scopes++;
        return $itercode;
    }
    my $code = &ml2pl($_[0]);
    $_[0] = '';
    return "$itercode$code\n}";
}

sub GOTO ($) {
    if ($_[0] =~ s/^([a-z]\w*)\b//i) {
        return "goto __lbl_Mumps_$1;";
    }
    $_[0] =~ s/\s.*$//;
    die "Illegal label in GOTO: $_[0]";
}

sub HALT {
    return "exit;";
}

sub HANG ($) {
    return "exit;" unless ($_[0]);
    my ($code, $var) = &makeexp($_[0]);
    return $code . "sleep($var);";
}

sub IF ($) {
    die "Condition expected in IF" unless ($_[0]);
    my ($code, $val) = &makeexp($_[0]);
    my $condcode = $code . "\$Language::Mumps::flag = $val ? 1 : undef;\nif (\$Language::Mumps::flag) {\n";
    $_[0] =~ s/^\s*//;
    die "Code expected in IF" unless ($_[0]);
    if ($_[0] =~ s/^\{//) {
        $scopes++;
        return $condcode;
    }
    $code = &ml2pl($_[0]);
    $_[0] = '';
    return "$condcode$code\n}";
}

sub JOB {
    die "Not implemented: JOB";
}

sub KILL ($) {
    unless ($_[0]) {
        return "%Language::Mumps::symbols = ()";
    }
    my $rev;
    my $thecode;
    my $cond = "if";
    my $tmptbl = &nextvar();
    if ($_[0] =~ s/^\(//) {
        $rev = 1;
    }
    $thecode = "{ my \%$tmptbl;\n";
    while ($_[0] && $_[0] !~ /^\s/) {
        last if ($rev && $_[0] =~ s/^\)>//);
        die "Variable expected in KILL" unless ($_[0] =~ /^\^?\w/);
        my ($code, $var) = &makevar($_[0]);
        die "Can unkill only regular arrays" if ($rev && ref($var) !~ /var/i);
        my $addr = $var->addr;
        $thecode .= $code . (!$rev
                ?  $var->purge . "\n"
                : "&Language::Mumps::moveimage(\\\%Language::Mumps::symbol, \\\%$tmptbl, " .
                        "$addr);\n"
           );
    }
    if ($rev) {
        $thecode .= <<EOM;
\%Language::Mumps::symbol = ();
foreach (keys \%$tmptbl) {
    \$Language::Mumps::symbol{\$_} = \$$tmptbl\{\$_};
}
EOM
    }
    chomp $thecode;
    $thecode;
}

sub LOCK ($) {
    unless ($_[0]) {
    return <<EOM;
foreach (\@Language::Mumps::locks) {
    flock(\$_, 8);
}
\@Language::Mumps::locks = ();
EOM
    }
    my ($code, $var) = &makevar($_[0]);
    die "Only one array can be LOCKed" if ($_[0] && $_[0] !~ /^\s/);
    my $ext = $var->getdb;
    my $tdb = &nextvar('$');
    my $fd = &nextvar('$');
return <<EOM;
$tdb = $ext;
$fd = $tdb->fd;
die "LOCK: flock: $!" unless flock($fd, 6);
push(\@Language::Mumps::locks, $fd);
EOM
}

sub OPEN ($) {
    my $opennum = &nextvar('$');
    my $tokens = &nextvar('@');
    my $ofn = &nextvar('$');
    my $omet = &nextvar('$');
    my ($code, $var) = &makeexp($_[0]);
    die ": expected in OPEN" unless ($_[0] =~ s/^\://);
    $code .= "$opennum = $var;\n";
    my ($code2, $var2) = &makeexp($_[0]);
    $code . $code2 . <<EOM;
die "Can't reOPEN unit 5" if ($opennum == 5);
($ofn, $omet) = $tokens = split(/\\//, $var2);
die "Illegal OPEN string" unless (scalar($tokens) == 2 &&
    grep /^$omet\$/i, qw(NEW OLD APPEND));
\$Language::Mumps::handlers[$opennum] = "F" . $opennum;
open(\$Language::Mumps::handlers[$opennum],
    {NEW => '>', APPEND => '>>', OLD=> '<'}->{uc($omet)} . $ofn);
\$Language::Mumps::handlers[$opennum] = \*{\$Language::Mumps::handlers[$opennum]};
EOM
}

sub QUIT {
    return <<EOM;
if (\@Language::Mumps::stack) {
    goto &{pop \@Language::Mumps::stack};
}
exit;
EOM
}

sub READ ($) {
    my ($result, $timeout, $done);
    while ($_[0] && $_[0] !~ /^\s/) {
        die "Comma expected in READ" unless (!$done++ || $_[0] =~ s/^,//);
        if ($_[0] =~ /^\*?[a-z^]/i) {
            my $icode = "&Language::Mumps::read";
            if ($_[0] =~ s/^\*//) {
                $icode = "ord(&Language::Mumps::readkey)";
            }
            my ($code, $lvar) = &makevar($_[0]);
            my $var = $lvar->lval;
            $result .= "\$SIG{ALRM} = sub {die 1;}; \$\@ = undef; alarm $timeout;\n"
                . "eval {\n" if ($timeout);
            $result .= "$var = $icode;\n";
            $result .= "};\n\$SIG{ALRM} = undef; alarm 0;\n\$Language::Mumps::flag = (\$\@ ? undef : 1);\n" if ($timeout);
            $timeout = undef;
        } elsif ($_[0] =~ s/^\?(\d+)//) {
            $timeout = $1;
        } else {
            my ($code, $var) = &makeexp($_[0]);
            $result .= $code . "&Language::Mumps::write($var);\n";
        }
    }
    chomp $result;
    $result;
}

sub SET ($) {
    my ($result, $done);
    while ($_[0] && $_[0] !~ /^\s/) {
        die ", expected in SET" unless ($_[0] =~ s/^\=// || !$done++);
        my ($code, $lvar) = &makevar($_[0]);
        my $var = $lvar->lval;
        die "= expected in SET" unless ($_[0] =~ s/^\=//);
        my ($code2, $val) = &makeexp($_[0]);
        my $lval = &nextvar("");
        $result .= $code . "*$lval = \\$var;\n" .
                $code2 . "\$$lval = $val;\n*$lval = \\\$sysundef;\n";
    }
    $result;
}

sub USE ($) {
    my ($code, $val) = &makeexp($_[0]);
    return $code . <<EOM;
\$Language::Mumps::xreg[\$Language::Mumps::selected_io] = \$Language::Mumps::xpos;
\$Language::Mumps::yreg[\$Language::Mumps::selected_io] = \$Language::Mumps::ypos;
\$Language::Mumps::selected_io = $val;
\$Language::Mumps::xpos = \$Language::Mumps::xreg[\$Language::Mumps::selected_io];";
\$Language::Mumps::ypos = \$Language::Mumps::yreg[\$Language::Mumps::selected_io];";
EOM
}

sub VIEW {
    die "Not implemented: VIEW";
}

sub WRITE {
    my ($code, $val) = &makelist($_[0]);
    return $code . <<EOM;
foreach ($val) {
    &Language::Mumps::write(\$_);
}
EOM
}

sub XECUTE {
    my ($code, $val) = &makelist($_[0]);
    return $code . <<EOM;
foreach ($val) {
    eval &ml2pl($_);
    die "XECUTE: \$\@" if \$\@;
}
EOM
}

sub ZP ($) {
    my $line = $_[0];
    $_[0] = '';
    return "\$Language::Mumps::flag = ($line) ? 1 : undef;";
}

sub ZD ($) {
    my $line = $_[0];
    $_[0] = '';
    return $line;
}

sub ZFUNCTION ($) {
      my @tokens = ($_[0] =~ s/^\s*([a-z]\w*)(?:\(?:(?:([a-z]\w*)(\,[a-z]\w*)*)?\))?\s*$//i);
      die "Incorrect function header in ZFUNCTION" unless (@tokens);
      die "Cannot nest functions in ZFUNCTION" if ($infun++ > 1);
      my $fun = shift @tokens;
      $tmphash = &nextvar("");
      @tmpvars = @tokens;
      my $code .= "sub $fun {\nmy \%$tmphash;\n";
      foreach (@tokens) {
          my $obj = new Language::Mumps::var;
          $obj->name($_);
          my $var = $obj->lval;
          $code .= "\$$tmphash\{'$_'} = $var;\n$var = shift;\n";
      }
      $code;
}

sub ZRETURN ($) {
    die "Not in a function in ZRETURN" unless ($infun--);
    my ($code, $var) = &makeexp($_[0]);
    foreach (@tmpvars) {
          my $obj = new Language::Mumps::var;
          $obj->name($_);
          my $var = $obj->lval;
          $code .= "$var =\$$tmphash\{'$_'}\n";
    }
    $code . "return $var;\n}";
}

sub makevar ($) {
    my ($a, $b) = (0, 0);
    makevar2($_[0], $a, $b);
}

sub makevar2 ($$) {
    my ($code, $obj, $val, $var, $isfun, $extra);
    ++$_[1];
    if ($_[0] =~ s/^\$//) {
        $obj = new Language::Mumps::Func;
        $isfun = 1;
        $extra = '$';
    } elsif ($_[0] =~ s/^\^//) {
        $obj = new Language::Mumps::Database;
    } elsif ($_[0] =~ s/^\&//) {
        $obj = new Language::Mumps::Freevar;
    } else {
        $extra = '%';
        $_[0] =~ s/^\@//;
        $obj = new Language::Mumps::Var;
    }
    die "Illegal array name" unless ($_[0] =~ /[a-z$extra]/i);
    $_[0] =~ s/^([a-z$extra]\w*)//i;
    my $alias = $1;
    $alias = $FUNCTIONS{uc($alias)} || $alias if ($isfun);
    my $this;
    if ($_[0] =~ s/^\(//) {
        unless ($isfun) {
              ($code, $var) = &makelist2($_[0], $_[1], $_[2] + 1);
              die "No closing brackets" unless ($_[0] =~ /^\)/);
              goto regular;
        }
        if ($alias =~ s/^(\$)//) {
              ($code, $var) = &makelist2($_[0], $_[1], $_[2] + 1);
              bless $obj, 'Language::Mumps::Primitive';
              goto regular;
        }
        $alias =~ tr/a-z/A-Z/;
        my $opt = $FUNS{$alias};
        die "Illegal function $alias" unless (@$opt);
        my $line;
        foreach (@$opt) {
            $line = $_[0];
            $@ = undef;
            $obj->prot($_->{'prot'});
            eval {
                ($code, $var) = &makelist2($line, $_[1], $_[2] + 1,
                   $obj->prot);
                die "No closing brackets" unless ($line =~ /^\)/);
            };
            goto success unless ($@);
        }
        die "Unmatched function prototype for $alias: $@";
success:
        $_[0] = $line;
regular:
        $obj->list($var);
        die "No closing brackets" unless ($_[0] =~ s/^\)//);
    } elsif ($isfun) {
        $alias =~ tr/a-z/A-Z/;
        my $opt = $FUNS{$alias};
        die "Illegal function $alias" unless (@$opt);
        my $line;
        foreach (@$opt) {
            goto day unless ($_->{'prot'});
        }
        die "Function $alias requires parameters";
day:
    }
    $obj->name($alias);
    ($code, $obj);
}

sub makeexp ($) {
    my ($a, $b) = (0, 0);
    makeexp2($_[0], $a, $b);
}

sub makeexp2 ($$) {
    my ($step);
    my $scope = ++$_[1];
    my ($result, $sum);
    my $var = &nextvar('$');
    my $negation;
    while ($_[0] && $_[0] !~ /^(\,|\s|\:)/) {
        my ($val, $code);
        $_[0] =~ s/^(.)//;
        my $ch = $1;
        if ($ch eq '"') {
            my $flag;
            while (1) {
                $_[0] =~ s/^(.)//;
                my $ch = $1;
                last if ($ch eq '"' && !$flag);
                if ($ch eq '\\' && !$flag) {
                    $flag = 1;
                    next;
                }
                $ch = ($flag ? "\\$ch" : quotemeta($ch));
                $flag = undef;
                $val .= $ch;
                die "Unterminated string" unless ($_[0]);
            }
            $val = qq!"$val"!;
        } elsif ($ch eq '!') {
            $val = qq!"\\n"!;
        } elsif ($ch eq '#') {
            $val = qq!['cls']!;
        } elsif ($ch eq '?' && $result) {
            die "Regexp expected" unless ($_[0] =~ s/^(\S+)//);
            $val = &makeregexp($1);
            $result .= "$var = ($var =~ /^($val)\$/);\n";
            next;
        } elsif ($ch eq '?') {
            die "Tab expected" unless ($_[0] =~ s/^(\d+)//);
            $val = qq!['tab', $1]!;
        } elsif ($ch =~ /[0-9\.]/) {
            my ($exp, $dot);
            $val = $ch;
            while ($_[0] =~ s/^(\d+|\.|E)//i) {
                my $ch = $1;
                if ($ch eq '.') {
                    $dot++;
                    die "Illegal number" if ($dot > 1);
                }
                if (uc($ch) eq 'E') {
                    $exp++;
                    die "Illegal number" if ($exp > 1);
                }
                $val .= $ch;
            }
            die "Illegal number" unless ($val =~ /\d$/);
        } elsif ($ch =~ /[a-z\$\^\@\%\&]/i) {
            $_[0] = $ch . $_[0];
            ($code, $val) = &makevar2($_[0], $_[1], $_[2]);
            $val = $val->rval;
        } elsif ($ch =~ /['-]/) {
            $ch =~ s/'/!/;
            $negation = $ch;
            next;
        }
        if (defined($val)) {
            $result .= $code;
            $result .= "$var = $negation$val;\n";
            $result .= "$sum\n" if ($sum);
            $sum = undef;
            $negation = undef;
            next;
        }
        die "Left operand expected" if ($sum);
        if ($ch eq ')') {
            $_[0] = $ch . $_[0];
            last if ($_[2]);
            die "Unexpected right bracket";
        }
        if ($ch eq '(') {
            ($code, $val) = &makeexp2($_[0], $_[1], $_[2] + 1);
            die "No closing brackets" unless ($_[0] =~ /^\)/);
        }
        my $oldvar = $var;
        $var = &nextvar('$');
        my $qch = quotemeta($ch);
        if ("+-*/!&_" =~ /$qch/) {
            $ch =~ s/\!/||/;
            $ch =~ s/\&/&&/;
            $ch =~ s/_/./;
            $sum = "$var $ch= $oldvar;";
        }
        if ($ch eq '#') {
            $sum = "$var = $var % $oldvar;";
        }
        if ($ch eq "'") {
            if ($_[0] =~ /^\=\<\>/) {
                $_[0] =~ s/^(.)//;
                $ch = $1;
            }
        }
        if ("=<>" =~ /$qch/) {
            $ch =~ s/\=/==/;
            $sum = "$var = ($oldvar <=> $var) || ($var cmp $oldvar);\n" .
                   "$var = ($var $ch 0);";
        }
        if ($ch =~ /\[\]/) {
            my ($s1, $s2) = ($var, $oldvar);
            ($s2, $s1) = ($var, $oldvar) if ($ch eq '[');
            $sum = "$s2 = quotemeta($s2);\n$var = (($s1 =~ /$s2) ? 1 : undef);";
        }
        die "Parse error on $ch" unless ($sum)
    }
    die "Right operand expected" if ($sum);
    die "Right bracket expacted $_[2] $_[0]" if ($_[2] && $_[0] =~ /^\s/);
    ("$result", $var);
}

sub makelist ($) {
    my ($a, $b) = (0, 0);
    makelist2($_[0], $a, $b, $_[1]);
}

sub makelist2 ($$) {
    my ($step);
    my $scope = ++$_[1];
    my ($result, $sum);
    my $var = &nextvar('@');
    my $lbl = "__lbl_$var";
    $lbl =~ s/[^a-z]/_/g;
    my $i;
    my $first = 1;
    $result = "$var = ();\n";
    my $proto = $_[3];
    while ($_[0] && $_[0] !~ /^\s/) {
        die "Comma expected" unless ($first || $_[0] =~ s/^,//);
        die "Parameter mismatch" if ($_[3] && !$proto);
        my $typ;
        $typ = $1 if ($proto =~ s/^(.)//);
        $typ =~ s/[IL]//;
        $proto = 'L' if ($typ eq 'L');
        $proto = 'T' if ($typ eq 'T');
        my %procs = ("", sub($$) {&makeexp2($_[0], $_[1], $_[2])},
                  "O", sub ($$) {
                      my ($code, $var) = &makevar2($_[0], $_[1], $_[2]);
                      ($code, $var->sig);},
                  "T", sub ($$) {my ($code, $var2) = &maketuple2($_[0], $_[1], $_[2], 2, ":");
                     my ($cond, $res) = @$var2;
                     ("$code $var = ($res);\ngoto $lbl if ($cond);", "undef");
                  },
                  "S", sub ($$) {
   die "Source anchor expected" unless ($_[0] =~ s/^(\d+|(?:[a-z]\w*)?\+\d+|[a-z]\w*)//i);
                    my ($lbl, $off) = split(/\+/, $1);
                    $off *= 1;
                    my $var = &nextvar('$');
                    ("$var = &Language::Mumps::list('$lbl', $off);\n", $var);}
                  );
        my ($code, $val) = &{$procs{$typ}}($_[0], $_[1], $_[2]);
        $result .= $code . "push($var, $val);\n";
        ++$i;
        $first = undef;
        if ($_[0] =~ /^\)/) {
            last if ($_[2]);
            die "Unexpected right bracket";
        }
    }
    $result .= "$lbl: " if ($proto eq 'T');
    die "Expected right operand" if ($sum);
    ($result, $var, $i);
}

sub maketuple ($) {
    my ($a, $b) = (0, 0);
    maketuple2($_[0], $a, $b, $_[1], $_[2]);
}

sub maketuple2 ($$) {
    my ($done, $result);
    ++$_[1];
    my @ary;
    my $first = 1;
    my $delim = quotemeta($_[4]);
    foreach (1 .. $_[3]) {
        die "$_[4] expected" unless ($first || $_[0] =~ s/^$delim//);
        $first = undef;
        my ($code, $var) = &makeexp2($_[0], $_[1], $_[2]);
        my $save = &nextvar('$');
        $result .= $code . "$save = $var;\n";
        push(@ary, $var);
    }
    ($result, \@ary);
}

%RES = qw(A [a-zA-Z]
          C [\x0-\x1F0xFF]
          E [\x0-\x7F]
          H [\xE0-\xFA]
          L [a-z]
          N \d
          U [A-Z]);
my $s = pack("C*", (ord(' ') + 1 .. ord('a') - 1));
$s =~ tr/a-z0-9A-Z//;
$RES{'P'} = '[' . quotemeta($s) . ']';
$RESKEYS = join("", keys %RES);

sub makeregexp {
    my $result;
    my $src = shift;
    while ($src) {
        if ($src =~ s/^([$RESKEYS])//) {
            $result .= $RES{$1};
        } elsif ($src =~ s/^".*?"//) {
            $result .= quotemeta($1);
        } else {
            die "Invalid REGEXP char: " . substr($src, 0, 1);
        }

        if ($src =~ s/\.//) {
            $result .= '+';
        }
        if ($src =~ s/^(\d+)//) {
            $result .= "{$1}";
        }
    }
    $result;
}

sub nextvar {
    my $pre = shift;
    $varstack++;
    my $sc = "_" x $scopes;
    "$pre$sc\__tmp$varstack";
}

sub resetvars {
    $varstack = 0;
}

sub curse {
    require Curses;
    return unless (*Curses::new{CODE});
    Curses::initscr unless ($curses_inside++);
}

sub cls {
    if ($Language::Mumps::selected_io == 5) {
        &curse;
        Curses::clear;
    } else {
        &write("\l");
    }
    ($xpos, $ypos) = (0, 0);
}

sub readkey {
    &curse;
    Curses::getch;
}

sub read {
    my $file = ($selected_io == 5) ? \*STDIN : $handlers[$selected_io];
    my $s = scalar(<$file>);
    chomp $s;
    $xpos = 0;
    $ypos++;
    $s;
}

sub write {
    my $file = ($selected_io == 5) ? \*STDOUT : $handlers[$selected_io];
    my $item = shift;
    return unless (defined($item));
    if ($item->[0] eq ('cls')) {
        &cls;
        next;
    }
    if ($item->[0] eq 'tab') {
        &tab($item->[1]);
        next;
    }
    my @frags = ($item eq "\n" ? ('', '') : split(/\n/, $item));
    my $i;
    foreach (@frags) {
        print $file $_;
        $xpos = ($xpos + length($_));
        if (++$i < @frags) {
            print $file "\n";
            $xpos = 0;
            $ypos++;
        } 
    }
}

sub tab {
    my $to = shift;
    if ($xpos > $to) {
        &write("\n");
    }
    my $dist = $to - $xpos;
    &write(' ' x $dist);
}

sub import {
    my $class = shift;
    my $state;

    foreach $state (@_) {
        if ($state eq "Runtime") {
            tie %symbols, 'Language::Mumps::Tree';
            tie %dbs, 'Language::Mumps::Forest';
            $selected_io = 5;
        } elsif ($state =~ /^[SNG]?DBM?_File$/) {
            $@ = undef;
            eval "require $state; import $state;";
            die $@ if ($@);
            @TYING = (O_RDWR|O_CREAT, 0644,
                 ($state eq 'DB_File') ? ($DB_File::DB_HASH) : ());
            $DB = $state;
        } elsif ($state eq 'Data::Dumper') {
            $@ = undef;
            eval "require $state; import $state;";
            die $@ if ($@);
            $FETCH = sub {no strict; eval $_[0];};
            $STORE = \&Data::Dumper::Dumper;
            $SER = $state;
        } elsif ($state eq 'FreezeThaw' || $state eq 'Storable') {
            $@ = undef;
            eval "require $state; import $state;";
            die $@ if ($@);
            $FETCH = \&{"$SER\::thaw"};
            $STORE = \&{"$SER\::freeze"};
            $SER = $state;
        } elsif ($state eq 'XML') {
            $@ = undef;
            eval "require XML::Parser; import XML::Parser;";
            eval "require XML::Dumper; import XML::Dumper;";
            die $@ if ($@);
            $Language::Mumps::Pool::XML = new XML::Dumper;
            $FETCH = sub { 
                my $xml = shift;
                return undef unless ($xml);
                my $parser = new XML::Parser(Style => Tree);
                my $tree = $parser->parse($xml);
                $Language::Mumps::Pool::XML->xml2pl($tree); };
            $STORE = sub { $Language::Mumps::Pool::XML->pl2xml(shift); };
            $SER = $state;
        } elsif ($state eq 'Config') {
            require "/etc/pmumps.cf" if (-f "/etc/pmumps.cf");
            require "~/.pmumps" if (-f "~/.pmumps");
            import Language::Mumps ($DB, $SER);
        } else {
            die "Unrecognized option $state";
        }
    }
    $IMPORT = join(" ", grep /./, ($DB, $SER));
}

sub dbs {
    my $db = shift;
    my $dbt = "Language::Mumps::DB::_$db";
    my $dbf = "Language::Mumps::DB::Back::_$db";;
    unless (-d "global") { 
        mkdir "global", 0755 || die "Can't create global/: $!";
    }
    die "You must configure database storage" unless ($DB);
    tie(%$dbf, $DB, "global/$db.db", @TYING) || die "DB: $!";
    my $t = tie %$dbt, 'Language::Mumps::Tree', \%$dbf, $FETCH,
        $STORE;
    \%$dbt;
}

sub moveimage {
    my ($src, $dst, $key) = @_;
    $dst->{$key} = $src->{$key};
    my $t = tied(%$src);
    my @children = $t->query($key);
    foreach (@children) {
        &moveimage($src, $dst, "$key\0$_");
    }
}

package Language::Mumps::Tree;

sub CLEAR {
    my $self = shift;
    my $hash = $self->{'hash'};
    %$hash = ();
}

sub STORE {
    my ($self, $key, $val) = @_;
    my $hash = $self->{'hash'};
    my $store = $self->{'store'};
    my $fetch = $self->{'fetch'};
    my @tokens = split(/\0/, $key);
    my @addr;
    my $addr;
    do {
        my $this = shift @tokens;
        my $flag;
        my $base = &$fetch($hash->{$addr}) || ++$flag && {};
        $base->{'metadata'} ||= ++$flag && {};
        $base->{'metadata'}->{$this} ||= ++$flag;
        $hash->{$addr} = &$store($base) if ($flag);
        push(@addr, $this);
        $addr = join("\0", @addr);
    } while (@tokens);
    my $flag;
    my $base = &$fetch($hash->{$addr}) || ++$flag && {};
    ($base->{'data'} eq $val) || ++$flag && ($base->{'data'} = $val);
    $hash->{$addr} = &$store($base) if ($flag);
}

sub FETCH {
    my ($self, $key, $val) = @_;
    my $hash = $self->{'hash'};
    my $fetch = $self->{'fetch'};
    return undef unless ($hash->{$key});
    my $base = &$fetch($hash->{$key}) || {};
    $base->{'data'};
}

sub EXISTS {
    my ($self, $key) = @_;
    my $hash = $self->{'hash'};
    my $fetch = $self->{'fetch'};
    return undef unless ($hash->{$key});
    my $base = &$fetch($hash->{$key}) || {};
    (exists $base->{'data'});
}

sub query {
    my ($self, $key) = @_;
    my $hash = $self->{'hash'};
    my $store = $self->{'store'};
    my $fetch = $self->{'fetch'};
    my $base = &$fetch($hash->{$key}) || {};
    keys %{$base->{'metadata'}};
}

sub DELETE {
    my ($self, $key) = @_;
    my $hash = $self->{'hash'};
    my $store = $self->{'store'};
    my $fetch = $self->{'fetch'};
    my $base = &$fetch($hash->{$key}) || {};
    foreach (keys %{$base->{'metadata'}}) {
        $self->DELETE("$key\0$_");
    }
    delete $hash->{$key};
    unless ($key =~ s/\0([^\0]*)$//) {
        $key =~ s/^(.*)$//;
    }
    delete $hash->{$key}->{'metadata'}->{$1};
}

sub extrapolate {
    my ($self, $key) = @_;
    my @sons = $self->query($key);
    my %recur = map {$self->extrapolate($_);} @sons;
    $recur{$key} = $self->FETCH($key) if ($self->EXISTS($key));
    %recur;
}

sub FIRSTKEY {
    my $self = shift;
    $self->{'keys'} = {$self->extrapolate("")};
    $self->NEXTKEY;
}

sub NEXTKEY {
    my ($self, $lastkey) = @_;
    each %{$self->{'keys'}};
}

sub TIEHASH {
    my ($class, $hash, $fetch, $store) = @_;
    $fetch ||= sub {$_[0];};
    $store ||= sub {$_[0];};
    $hash ||= {};
    my $self = {'hash' => $hash, 'store' => $store, 'fetch' => $fetch};
    bless $self, $class;
}

package Language::Mumps::Entity;

sub new {
    bless {}, shift;
}

sub case {
    my $class = ref(shift);
    ${$class . "::CASE"};
}

sub name {
    my $self = shift;
    $self->{'name'} = shift if (@_);
    $self->case ? $self->{'name'} : uc($self->{'name'});
}

sub list {
    my $self = shift;
    $self->{'list'} = shift if (@_);
    $self->{'list'} || '()';
}

sub isatom {
    my $self = shift;
    $self->{'list'} ? undef : 1;
}

sub rval {
    my $self = shift;
    $self->lval;
}

sub lval {
    my $self = shift;
    '${' . $self->hash . '}{' . $self->addr . '}';
}

sub purge {
     die "Abstract";
}

sub hash {
     die "Abstract";
}

sub addr {
     die "Abstract";
}

sub sig {
    my $self = shift;
    "(bless [" . $self->hash . ", " . $self->addr . "], 'varsig')";
}

package Language::Mumps::Var;
use vars qw(@ISA);
@ISA = qw(Language::Mumps::Entity);


sub purge {
    my $self = shift;
    my $list = $self->list;
    my $name = $self->name;
    "delete \$Language::Mumps::symbols{'$name', $list};";
}

sub hash {
    "Language::Mumps::symbols";
}

sub addr {
    my $self = shift;
    my $list = $self->list;
    my $name = $self->name;
    $self->isatom ? "'$name'" : qq!join("\\0", '$name', $list)!;
}

package Language::Mumps::Primitive;
use vars qw(@ISA $CASE);
@ISA = qw(Language::Mumps::Entity);
$CASE = 1;

sub lval {
    die "Can't use functions as Lvalue";
}

sub rval {
    my $self = shift;
    my $name = $self->name;
    my $list = $self->list;
    "$name($list);";
}

package Language::Mumps::Database;
use vars qw(@ISA);
@ISA = qw(Language::Mumps::Entity);

sub purge {
    my $self = shift;
    my $list = $self->list;
    my $name = $self->name;
    "delete \$Language::Mumps::dbs{'$name'}->{$list}";
}

sub getdb {
    my $self = shift;
    my $name = $self->name;
    "tied(\%{tied(\$Language::Mumps::dbs{'$name'})->{'hash'}})";
}

sub hash {
    my $self = shift;
    my $name = $self->name;
    "\$Language::Mumps::dbs{'$name'}";
}

sub addr {
    my $self = shift;
    my $list = $self->list;
    qq!join("\\0", $list)!;
}

package Language::Mumps::Freevar;
use vars qw(@ISA $CASE);
@ISA = qw(Language::Mumps::Entity);
$CASE = 1;

sub lval {
    my $self = shift;
    my $name = $self->name;
    $self->isatom ? "\$$name" : $self->SUPER::lval;
}

sub hash {
    my $self = shift;
    $self->name;
}

sub addr {
    my $self = shift;
    my $list = $self->list;
    qq!join("\\0", $list)!;
}

package Language::Mumps::Func;
use vars qw(@ISA @zwi_tokens);
@ISA = qw(Language::Mumps::Entity);

sub prot {
    my $self = shift;
    $self->{'prot'} = shift if (@_);
    $self->{'prot'};
}

sub lval {
    my $self = shift;
    my $name = $self->name;
    my $prot = $self->prot;
    my $opt = $Language::Mumps::FUNS{$name};
    my $rec;
    foreach $rec (@$opt) {
        last if ($rec->{'prot'} eq $prot);
    }
    die "Lvalue unavailable for function $name" unless ($rec->{'lval'});
    &{"l_$name"}($self);
}

sub rval {
    my $self = shift;
    my $name = $self->name;
    my $list = $self->list;
    "&Language::Mumps::Func::$name($list)";
}


sub ASCII {
    my ($str, $pos) = @_;
    $pos -= ($pos && 1);
    my $ch = substr($str, $pos, 1);
    $ch ? -1 : ord($ch);
}

sub CHAR {
    pack("C*", @_);
}

sub DATA {
    my ($hash, $addr) = @{$_[0]};
    my $d0 = defined($hash->{$addr});
    my $d1 = scalar(tied(%$hash)->query($addr));
    $d1 * 10 + $d0;
}

sub EXTRACT {
    my ($str, $from, $to) = @_;
    $to ||= $from;
    substr($str, $from - 1, $to - $from + 1);
}

sub FIND {
    my ($str, $sub, $pos) = @_;
    $pos -= ($pos && 1);
    index($str, $sub, $pos);
}

sub HOROLOG {
    my $years = 1970 - 1841;
    my $leaps = int($years / 4) - 1;
    my $distance = 1 + 365 * $years + $leaps;
    my $now = time;
    my @here = localtime($now);
    my @gmt = gmtime($now);
    my $here = $here[1] + 60 * $here[2];
    my $gmt = $gmt[1] + 60 * $gmt[2];
    my $offset = 60 * ($here - $gmt);
    my $there = $now + $offset;
    my $n1 = int($there / 3600 / 24) + $distance;
    my $n2  = $gmt * 60 + $gmt[0];
    "$n1,$n2";
}

sub IO {
    $Language::Mumps::selected_io;
}

sub lIO {
    '$Language::Mumps::selected_io';
}

sub JOB {
    $$;
}

sub JUSTIFY {
    my ($str, $ln, $dec) = @_;
    $str = sprintf("%.$dec\d", $str) if ($dec);
    my $l = $ln - length($str);
    ($l > 0 ? (" " x $ln) : "") . $str;
}

sub LEN {
    my ($str, $token) = @_;
    $token = quotemeta($token) || ".";
    scalar($str =~ s/($token)//g);
}

sub NEXT {
    my ($hash, $addr) = @{$_[0]};
    my @tokens = split(/\0/, $addr);
    my $right = pop @tokens;
    my @sons = sort (tied(%$hash)->query(join("\0", @tokens)));
    return -1 unless (@sons);
    return $sons[0] if ($right == -1);
    foreach (@sons) {
        return $_ if ($_ gt $right);
    }
    return -1;
}

sub ORDER {
    my ($hash, $addr) = @{$_[0]};
    my @tokens = split(/\0/, $addr);
    my $right = pop @tokens;
    my @sons = sort {$a <=> $b} @{tied(%$hash)->query(join("\0", @tokens))};
    foreach (@sons) {
        return $_ if ($_ ge $right || $right == -1);
    }
    return -1;
}

sub PIECE {
    my ($str, $delim, $from, $to) = @_;
    if (ref($str) eq 'varsig') {
        my ($hash, $addr) = @$str;
        $str = $hash->{$addr};
    }
    my $qdelim = quotemeta($delim);
    my @tokens = split(/$qdelim/, $str);
    $to ||= $from;
    join($delim, @tokens[($from - 1) .. ($to - 1)]);
}

sub lPIECE {
    my $list = shift;
    "\${&Language::Mumps::Func::tiePIECE($list)}";
}

sub tiePIECE {
    my $scalar;
    tie $scalar, 'Language::Mumps::Piece', @_;
    \$scalar;
}

sub RANDOM {
    my $max = shift;
    int(rand($max));
}

sub SELECT {
    $_[0];
}

sub TEST {
    $Language::Mumps::flag;
}

sub lTEST {
    '$Language::Mumps::flag';
}

sub TEXT {
    $_[0];
}

sub X {
    \$Language::Mumps::xreg[\$Language::Mumps::selected_io]
}

sub Y {
    \$Language::Mumps::yreg[\$Language::Mumps::selected_io]
}

sub ZAB {
    abs(shift);
}

sub ZB {
    $_ = shift;
    s/^\s*//;
    s/\s*$//;
    s/\s+/ /;
    $_;
}

sub ZCD {
    my $fn = shift || substr(time, 0, 8) . ".dmp";
    my $forest = {};
    $! = undef;
    foreach ((glob "global/*.db"), (glob "global/*.db.*")) {
        s|^global/||;
        s/\.db(\..*)?$//;
#        next if ($forest->{$_});
        eval {
            $forest->{$_} = {%{$Language::Mumps::dbs{$_}}};
        };
    }
    open(DUMP, ">$fn");
    print DUMP &$Language::Mumps::STORE($forest);
    close(DUMP);
    %Language::Mumps::dbs = ();
    $fn;
}

sub ZCL {
    my $fn = shift || "dump";
    %Language::Mumps::dbs = ();
    open(LOAD, $fn);
    binmode LOAD;
    my $buffer;
    while (read(LOAD, $buffer, 8192, length($buffer))) {}
    close(LOAD);
    my $forest = &$Language::Mumps::FETCH($buffer);
    undef $buffer;
    foreach (keys %$forest) {
        unlink "global/$_.db";
        %{$Language::Mumps::dbs{$_}} = %{$forest->{$_}};
    }
    %Language::Mumps::dbs = ();
}

sub ZD {
    scalar(localtime);
}

sub ZD1 {
    time;
}

sub ZD2 {
    scalar(localtime(shift));
}

sub ZD3 {
    my ($y, $m, $d) = @_;
    require Time::Local;
    my $t = Time::Local::timelocal(0, 0, 0, $d, $m - 1, $y - 1900);
    my @t = localtime($t);
    $t[7] + 1;
}

sub ZD4 {
    my ($y, $dy) = @_;
    my @mon = qw(31 28 31 30 31 30 31 31 30 31 30 31);
    my $m;
    while ($dy > $mon[$m]) {$dy -= $mon[$m++];}
    join(" ", $y, $m + 1, $dy);
}

sub ZD5 {
    my ($y, $m, $d) = @_;
    require Time::Local;
    my $t = Time::Local::timelocal(0, 0, 0, $d, $m - 1, $y - 1900);
    my @t = localtime($t);
    join(",", $y, $t[7] + 1, $t[6]);
}

sub ZD6 {
    my $t = (shift) || time;
    my @t = localtime($t);
    sprintf("%2d:%02d", $t[2], $t[1]);
}

sub ZD7 {
    my $t = (shift) || time;
    my @t = localtime($t);
    join("-", $t[5] + 1900, $t[4] + 1, $t[3]);
}

sub ZD8 {
    my $t = shift;
    &ZD7($t) . "," . &ZD6($t);
}

sub ZD9 {
    my $t = (shift) || time;
    my @t = localtime($t);
    join(",", &ZD7($t), $t[6], &ZD6($t));
}

sub ZDBI {
    my ($dsn, $u, $p, $query, $ary) = 2_;
    require DBI;
    import DBI;
    my $dbh = DBI->connect($dsn, $u, $p);
    my $sth = $dbh->prepare($query) || die $DBI::errstr;
    $sth->execute || die $DBI::errstr;
    my ($i, $rec, $glb);
    $glb = $Language::Mumps::dbs{$1} if ($ary =~ /^\^(.*)$/);
    
    while ($rec = $sth->fetchrow_array) {
        $Language::Mumps::symbol{"%tpl", ++$i} = join("\\", @$rec);
        unless ($glb) {
            $Language::Mumps::symbol{$ary, @$rec} = $i;
        } else {
            $glb->{@$rec} = $i;
        }
    }
    $sth->finish;
    $i;
}

sub ZF {
    (-f shift);
}

sub ZH {
    my $s = shift;
    $s =~ s/([^ a-zA-Z0-9])/sprintf("%%%02x", $1)/ge;
    $s =~ s/ /+/g;
    $s;
}

sub ZL {
    my ($a1, $a2) = @_;
    return ln($a1) unless (defined($a2));
    substr($a1 . (" " x $a2), 0, $a2);
}

sub ZN {
    my $s = uc(shift);
    $s =~ s/\W//g;
    $s;
}

sub ZP {
    my ($a1, $a2) = @_;
    substr($a1 . (" " x $a2), 0, $a2);
}

sub ZR {
     sqr(shift);
}

sub ZS {
    &Language::Mumps::write(`$_[0]`);
}

sub ZSQR {
     my $x = shift;
     $x * $x;
}

sub ZT {
    my $file = ($Language::Mumps::selected_io == 5) ? \*STDIN : $Language::Mumps::handlers[$Language::Mumps::selected_io];
    tell($file);
}

sub ZVARIABLE {
    ${scalar(caller) . '::' . $_[0]};
}

sub ZV1 {
    $_[0] =~ /^[a-z]\w*$/;
}

sub ZWI {
    @zwi_tokens = split(/\s+/, shift);
}

sub ZWN {
    shift @zwi_tokens;
}

package Language::Mumps::Piece;

sub TIESCALAR {
    my $class = shift;
    bless [@_], $class;
}

sub FETCH {
    my $self = shift;
    &Language::Mumps::Func::PIECE(@$self);
}

sub STORE {
    my ($self, $val) = @_;
    my ($var, $delim, $from) = @$self;
    my ($hash, $addr) = @$var;
    my $str = $hash->{$addr};
    $delim = quotemeta($delim);
    my @tokens = split(/$delim/, $str);
    $tokens[$from - 1] = $val;
    $str = join($delim, @tokens);
    $hash->{$addr} = $str;
}


package Language::Mumps::Forest;

sub TIEHASH {
    bless {'dbs' => {}}, shift;
}

sub FETCH {
    my ($self, $key) = @_;
    my $dbs = $self->{'dbs'};
    $dbs->{$key} ||= &Language::Mumps::dbs($key);
    $dbs->{$key};
}

sub DELETE {
    my ($self, $key) = @_;
    my $dbs = $self->{'dbs'};
    my $hash = $dbs->{$key};
    untie %$hash;
}

sub CLEAR {
    my ($self, $key) = @_;
    my $dbs = $self->{'dbs'};
    my $hash;
    foreach $hash (keys %$dbs) {
        untie %$hash;
    }
    delete $self->{'dbs'};
}
__END__

__END__
# Documentation
=head1 NAME

Mumps - Perl module to translate Mumps programs to perl scripts

=head1 SYNOPSIS

use Language::Mumps;

$pcode = Language::Mumps::compile(qq{\tw "Hello world!",!\n\th});
eval $pcode;

Language::Mumps::evaluate(qq{\ts x=1 w x});

Language::Mumps::interprete("example.mps");

Mumps:translate("example.mps", "example.pl");
B<prompt %> C<perl example.pl>

=head1 DESCRIPTION

This module compiles Mumps code to Perl code. The API is simillar to
MumpsVM.

=head1 ENVIRONMENT

Edit ~/.pmumps or /etc/pmumps to set up persistent arrays.

=head1 FILES

=over 6

=item F<$BINDIR/pmumps>
 Interpreter

=item F<~/.pmumps>
 User configuration

=item F</etc/pmumps.cf>
 Site configuration

=back

=head1 AUTHOR

Ariel Brosh, B<schop@cpan.org>

=head1 SEE ALSO

L<pmumps>, L<DB_File>.  
