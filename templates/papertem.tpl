%%% TEMPLATE.tex.tpl --- (>>>POINT<<<)
%%% Time-stamp: < >
%% Author: (>>>AUTHOR<<<)
%% Version: $Id: (>>>FILE<<<),v 0.0 (>>>DATE<<<) (>>>TIME<<<) (>>>LOGIN_NAME<<<) Exp$
%%\revision$Header: (>>>DIR<<<)(>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp$
\NeedsTeXFormat{LaTeX2e}
\documentclass[a4paper,oneside,fleqn,12pt]{article}
\usepackage[tbtags]{amsmath}
\usepackage{amssymb,amsthm,graphicx}
\usepackage{rotating,txfonts}
\usepackage{mathrsfs}
\usepackage{listings,xcolor}
%\usepackage[author-year]{amsrefs}
\usepackage[round,comma]{natbib}
\usepackage{setspace}
\usepackage[pdfauthor          = {Shusong~JIN},
%            pdftitle          = {(>>>4<<<)},
            bookmarks          = false,
            pdfdisplaydoctitle = true,
            bookmarksnumbered,
            colorlinks,
            linktocpage,
			citecolor  = black,
            linkcolor  = black]{hyperref}
\setlength{\textwidth}{145mm}
\setlength{\textheight}{225mm}
\setlength{\oddsidemargin}{10mm} %the true margin is 1in+(-.5cm)
\setlength{\evensidemargin}{10mm}
%\setlength{\topmargin}{-0.5in} %the true margin is 1in+(-.5cm)
%\setlength{\headheight}{1.5cm}
%\setlength{\headsep}{1.5cm} %the distance from the bottom of headline to top of body
%\setlength{\topskip}{1cm} % distance from top of body to baseline of first line of text
%\setlength{\mathindent}{2.5cm}
%\setlength{\parindent}{1.2em}
\setlength{\parskip}{1.5em plus0.3em minus0.3em}
\numberwithin{equation}{section}
\theoremstyle{plain}
\newtheorem{theorem}{Theorem}[section]
\newtheorem{lemma}[theorem]{Lemma}
\newtheorem{proposition}[theorem]{Proposition}
\newtheorem{corollary}{Corollary}[section]
\theoremstyle{definition}
\newtheorem{definition}{Definition}%[section]
\newtheorem{conjecture}{Conjecture}%[section]
\newtheorem{assumption}{Assumption}%[section]
\newtheorem{example}{Example}%[section]
\theoremstyle{remark}
\newtheorem*{remark}{Remark}
\newtheorem*{note}{Note}
\newtheorem{case}{Case}
\newcommand{\pcon}{\xrightarrow{p}}
\newcommand{\ascon}{\xrightarrow{a.s.}}
\newcommand{\dcon}{\xrightarrow{\mathscr{L}}}
\newcommand{\cdf}{{\scshape cdf }}
\newcommand{\pdf}{{\scshape pdf }}
\newcommand{\iid}{{\scshape iid }}
\newcommand{\MSE}{{\scshape mse }}
%\renewcommand{\theequation}{\thesection.\arabic{equation}}
%\renewcommand{\baselinestretch}{1.4}
%\singlespacing
%\onehalfspacing
\doublespacing
\DeclareMathOperator{\e}{\mathbf{E\, }}
\DeclareMathOperator{\var}{var}
\DeclareMathOperator{\corr}{corr\, }
\DeclareMathOperator{\cov}{cov\, }
\DeclareMathOperator{\ve}{vec\, }
\DeclareMathOperator{\vech}{vech\, }
\DeclareMathOperator{\mse}{MSE\,}
\DeclareMathOperator*{\argmax}{arg\,max}
\DeclareMathOperator*{\argmin}{arg\,min}
\DeclareMathOperator*{\argsup}{arg\,sup}
\DeclareMathOperator*{\arginf}{arg\,inf}
\DeclareMathOperator*{\plim}{plim}
\providecommand{\norm}[1]{\lvert\!\lvert #1 \rvert\!\rvert}
\providecommand{\abs}[1]{\lvert #1 \rvert}
\title{(>>>1<<<)}
%%%\author{}
\date{}
\begin{document}
\maketitle
%%%% #######################################################
\section{(>>>2<<<)}

%%%% #######################################################

%%%% #######################################################
\newpage
%\bibliographystyle{amsxport}
\bibliographystyle{plainnat}
\bibliography{>>>3<<<}
%%%% #######################################################
\end{document}

