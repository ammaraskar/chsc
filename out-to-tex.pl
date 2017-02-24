$header = <<'EOF';
\documentclass[9pt]{article}

\usepackage[a4paper,margin=1in,landscape]{geometry}

\usepackage[T1]{fontenc}
\usepackage{palatino}
\usepackage[scaled=0.85]{beramono}
\usepackage{alltt}

\begin{document}
EOF

$footer = <<'EOF';
\end{document}
EOF


print $header;

$even = 1;

while (<>) {
  if (/^Flags/) {
    $_ =~ s/Args:.*//;
    print '\begin{alltt}', "\n";
    print "$_";
    print '\end{alltt}', "\n";
    print "\n";
  }
  if (/^Filename/) {
    print '\begin{tabular}{c|c|c|c|c|c|c|c}', "\n";
    print "$_";
    print '\hline', "\n";
  }
  if (/^[a-z].*\&.*\&.*\&.*\&.*\&/) {
    print "$_";
  }
  if (/^x2n1/) {
    print '\end{tabular}', "\n";
    print "\n";
    $even = ! $even;
    print '\clearpage', "\n\n" if $even;
  }
}

print $footer;

