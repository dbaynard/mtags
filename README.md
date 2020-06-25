# mtags

Generate ctags-compatible tags for (pandoc) markdown files.

I’ve been a little disappointed with markdown ctags support I’ve found.
The `mtags` executable in this project should process a pandoc markdown file and create tags corresponding to the following elements:

-   Section titles
-   Figures
-   Tables
