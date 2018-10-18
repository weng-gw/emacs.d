%%% TEMPLATE.tex.tpl --- (>>>POINT<<<)
%%% Time-stamp: <2006-04-06 15:33:25 jinss>
%% Author: (>>>LOGIN_NAME<<<)
%% Version: $Id: (>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp$
%%\revision$Header: (>>>DIR<<<)(>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp$
\NeedsTeXFormat{LaTeX2e}
\documentclass[10pt,cjk,hyperref={dvips}]{beamer}

\usetheme{PaloAlto}
\usepackage{times}
\usefonttheme{serif}
\usepackage{listings}
\usepackage{verbatim,color}
\usepackage{pst-node}
\usefonttheme{serif}
\setbeamercovered{transparent = 50}
\usepackage{amsmath,amssymb}
\usepackage{CJK}
\theoremstyle{plain}
\begin{CJK*}{GBK}{song}
\newtheorem{thm}{����}
\newtheorem{lem}{����}
\newtheorem{prop}{����}
\newtheorem*{cor}{����}
\end{CJK*}
\theoremstyle{definition}
\begin{CJK*}{GBK}{song}
\newtheorem{dfn}{����}
\newtheorem{ass}{��������}
\newtheorem{exm}{����}
\end{CJK*}
\theoremstyle{remark}
\begin{CJK*}{GBK}{song}
\newtheorem*{rmk}{ע��}
\end{CJK*}
\DeclareMathOperator{\e}{E}
\DeclareMathOperator{\var}{var}
\DeclareMathOperator{\corr}{corr}
\DeclareMathOperator{\cov}{cov}
\DeclareMathOperator*{\argsup}{arg\,sup}
\DeclareMathOperator*{\arginf}{arg\,inf}
\logo{\includegraphics[height=0.125\textwidth]{fdlogo.eps}} %fdlogo for demo, fdlogo2 for print
\begin{document}
\begin{CJK*}{GBK}{song}
\title[(>>>1<<<)]{(>>>2<<<)}

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
\author{������}

\institute[����ͳ��ϵ]{������ѧ����ѧԺͳ��ѧϵ}

\date{}

\frame{\titlepage}
%\section{Outline}
\frame{\tableofcontents}


\end{CJK*}
\end{document}
