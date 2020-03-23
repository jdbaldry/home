\LoadClass[8pt, a4paper]{article}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{cv}[2018/11/18 Personal CV]

\RequirePackage[margin=0.5in]{geometry}

\RequirePackage{titlesec}
\titleformat{\section}
  {\Large\scshape\raggedright}
  {}{0em}
  {}
  [\titlerule]

\titleformat{\subsection}
  {\large\scshape\raggedright}
  {}{0em}
  {}


% Remove paragraph indents.
\setlength{\parindent}{0pt}

% Increase paragraph spacing.
\setlength{\parskip}{2mm plus1mm minus1mm}

% Add dates to sections and subsections.
\newcommand{\datedsection}[3]{
  \section[#1]{#1 \hfill #2}
}
\newcommand{\datedsubsection}[3]{
  \subsection[#1]{#1 \hfill #2}
}
