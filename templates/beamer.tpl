%%% TEMPLATE.tex.tpl --- (>>>POINT<<<)
%%% Time-stamp: <>
%% Author: (>>>LOGIN_NAME<<<)
%% Version: $Id: (>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp$
%%\revision$Header: (>>>DIR<<<)(>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp$
\NeedsTeXFormat{LaTeX2e}
\documentclass[10pt,
               hyperref={dvips},
			   handout
			   ]{beamer}
\mode<article> % only for the article version
{
  \usepackage{fullpage}
  \usepackage{hyperref}
}

\mode<presentation>
{
  \setbeamertemplate{background canvas}[vertical shading][bottom=red!10,top=blue!10]
  \usetheme{PaloAlto}
  \usefonttheme[onlysmall]{structurebold}
}
\setbeamercolor{math text}{fg=red!50!black}
\setbeamercolor{normal text in math text}{parent=math text}
\setbeamercovered{dynamic}
\usepackage{times}
\usepackage{listings}
\usepackage{verbatim,color}
\usepackage{pst-node}
\usepackage{mathrsfs}
\usefonttheme[onlymath]{serif}
\setbeamercovered{transparent = 10}
\usepackage{amsmath,amssymb}
\usepackage{mathrsfs}
\theoremstyle{remark}
\newtheorem{remark}{Remark}
\DeclareMathOperator{\e}{E}
\DeclareMathOperator{\var}{var}
\DeclareMathOperator{\corr}{corr}
\DeclareMathOperator{\cov}{cov}
\DeclareMathOperator*{\argsup}{arg\,sup}
\DeclareMathOperator*{\arginf}{arg\,inf}
\DeclareMathOperator*{\argmax}{arg\,max}
\DeclareMathOperator*{\argmin}{arg\,min}
\newcommand{\pcon}{\xrightarrow{p}}
\newcommand{\ascon}{\xrightarrow{a.s.}}
\newcommand{\dcon}{\xrightarrow{\mathscr{L}}}
\newcommand{\iid}{{\scshape iid }}
\newcommand{\cdf}{{\scshape cdf }}
\newcommand{\pdf}{{\scshape pdf }}
\providecommand{\norm}[1]{\vert\!\vert #1 \vert\!\vert}
\providecommand{\abs}[1]{\lvert #1 \rvert}
\logo{\includegraphics[height=0.125\textwidth]{fdlogo}} %fdlogo for demo, fdlogo2 for print
\begin{document}
% \lstset{language = SAS}
% \defverbatim[colored]\airline{
% \begin{lstlisting}[Airline Model]
% title1 'International Airline Passengers'; 
% title2 '(Box and Jenkins Series-G)'; 
% data seriesg; 
%    input x @@; 
%    xlog = log( x ); 
%    date = intnx( 'month', '31dec1948'd, _n_ ); 
%    format date monyy.; 
%    datalines; 
% 112 118 132 129 121 135 148 148 136 119 104 118 
% 115 126 141 135 125 149 170 170 158 133 114 140 
% 145 150 178 163 172 178 199 199 184 162 146 166 
% 171 180 193 181 183 218 230 242 209 191 172 194 
% 196 196 236 235 229 243 264 272 237 211 180 201 
% 204 188 235 227 234 264 302 293 259 229 203 229 
% \end{lstlisting}
% }
\title[(>>>1<<<)]{(>>>2<<<)}
%\subtitle{>>>3<<<}
\author[S. Jin]{Shusong Jin \\ \texttt{jinss@fudan.edu.cn}}

\institute[Dept. of Stat.]{Department of Statistics\\
School of Management\\
Fudan University\\
Handan Rd. 220, Shanghai 200433}

\date{\today}

\frame{\titlepage}
\section<presentation>*{Outline}
\begin{frame}
  \frametitle{Outline}
  \tableofcontents[part=1,pausesections]
\end{frame}

\AtBeginSection[]
{
  \begin{frame}<beamer>
    \frametitle{Outline}
    \tableofcontents[currentsection]
  \end{frame}
}
\AtBeginSubsection[]
{
  \begin{frame}<beamer>
    \frametitle{Outline}
    \tableofcontents[currentsection,currentsubsection]
  \end{frame}
}

\part<presentation>{Main Talk}

%\section{Bibliography}
%\begin{frame}[allowframebreaks] \frametitle{Bibliography}
%\small{
%   \begin{thebibliography}
%   \beamertemplatebookbibitems
%   \beamertemplatearticlebibitems
%   \end{thebibliography}
%}
%\end{frame}

\end{document}
