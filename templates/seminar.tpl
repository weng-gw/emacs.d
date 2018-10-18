%%% TEMPLATE.tex.tpl --- (>>>POINT<<<)
%%% Time-stamp: < >
%% Author: (>>>LOGIN_NAME<<<)
%% Version: $Id: (>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp$
%%\revision$Header: (>>>DIR<<<)(>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp$
\NeedsTeXFormat{LaTeX2e}
\documentclass[a4,portrait]{seminar}
\usepackage{indentfirst}
\usepackage{amsmath}
\usepackage{graphicx}
\usepackage{pstcol}
\usepackage{semcolor}
\usepackage{pst-grad}
\usepackage{slidesec}
\usepackage{fancyhdr}

\newcommand{\SlideColours}[3][black]{%
% #1 = frame line color (optional, default=black),
% #2 = foreground color, #3 = background color
\slideframe[\psset{linecolor=#1,fillcolor=#3,fillstyle=solid}]{scplain}
\color{#2}}

% To avoid that the headers be too close of the top of the page
\renewcommand{\slidetopmargin}{2cm}

% To center horizontally the headers and footers (see seminar.bug)
\renewcommand{\headwidth}{\textwidth}

\definecolor{skyblue}{rgb}{0.5,0.6,1}
\definecolor{deepskyblue}{rgb}{0.3,0.3,0.5}
\definecolor{Pink}{rgb}{1.,0.75,0.8}
\definecolor{Gold}{rgb}{1.,0.84,0.}

\slideframe[\psset{fillstyle=gradient,gradmidpoint=0.5,
                   gradbegin=deepskyblue,gradend=skyblue}]{scplain}


\begin{document}
\begin{slide}

\title{AAAA}
\author{JJJJ}
\maketitle

\end{slide}

\begin{slide}

par1
\end{slide}


\begin{slide}

par2
\end{slide}

\end{document}
