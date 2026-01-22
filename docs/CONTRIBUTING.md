# Contributing

First of all, thanks for thinking about contributing to the *wdpar R*
package! This is an open source project, and contributions are extremely
welcome.

## How you can contribute

There are several ways you can contribute to this project. If you want
to know more about why and how to contribute to open source projects
like this one, see this [Open Source
Guide](https://opensource.guide/how-to-contribute/).

### Ask a question

Using the package and got stuck? Browse the
[documentation](https://prioritizr.github.io/wdpar) to see if you can
find a solution. Still stuck? Post your question as an [issue on
GitHub](https://github.com/prioritizr/wdpar/issues/new). While we cannot
offer user support, we’ll try to do our best to address it, as questions
often lead to better documentation or the discovery of bugs.

Want to ask a question in private? Contact the package maintainer by
[email](mailto:jeffrey.hanson@uqconnect.edu.au).

### Propose an idea

Have an idea for a new feature? Take a look at the
[documentation](https://prioritizr.github.io/wdpar) and [issue
tracker](https://github.com/prioritizr/wdpar/issues) to see if it isn’t
included or suggested yet. If not, suggest your idea as an [issue on
GitHub](https://github.com/prioritizr/wdpar/issues/new). While we can’t
promise to implement your idea, it helps to:

- Explain in detail how it would work.
- Keep the scope as narrow as possible.

See below if you want to contribute code for your idea as well.

### Report a bug

Using the package and discovered a bug? That’s annoying! Don’t let
others have the same experience and report it as an [issue on
GitHub](https://github.com/prioritizr/wdpar/issues/new) so we can fix
it. A good bug report makes it easier for us to do so, so please
include:

- Your operating system name and version (e.g. Mac OS 10.13.6).
- Details about your local setup that might be helpful in
  troubleshooting, such as your session information (via
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html)).
- Detailed steps to reproduce the bug.

### Improve the documentation

Noticed a typo on the website? Think a function could use a better
example? Good documentation makes all the difference, so your help to
improve it is very welcome!

#### The website

[This website](https://prioritizr.github.io/wdpar) is generated using
the [*pkgdown*](http://pkgdown.r-lib.org/) package. That means we don’t
have to write any html: content is pulled together from documentation in
the code, vignettes,
[Markdown](https://guides.github.com/features/mastering-markdown/)
files, the package `DESCRIPTION` and `_pkgdown.yml` settings. If you
know your way around `pkgdown`, you can [propose a file
change](https://help.github.com/articles/editing-files-in-another-user-s-repository/)
to improve documentation. If not, [report an
issue](https://github.com/prioritizr/wdpar/issues/new) and we can point
you in the right direction.

#### Function documentation

Functions are described as comments near their code and translated to
documentation using the [*roxygen2*](https://github.com/r-lib/roxygen2)
package. If you want to improve a function description:

1.  Go to `R/` directory in the [code
    repository](https://github.com/prioritizr/wdpar).
2.  Look for the file with the name of the function.
3.  [Propose a file
    change](https://help.github.com/articles/editing-files-in-another-user-s-repository/)
    to update the function documentation in the roxygen comments
    (starting with `#'`).

### Contribute code

Care to fix bugs or implement new functionality? That’s brilliant! Have
a look at the [issue list](https://github.com/prioritizr/wdpar/issues)
and leave a comment on the things you want to work on. See also the
development guidelines below.

## Development guidelines

We try to follow the [GitHub
flow](https://guides.github.com/introduction/flow/) for development.

1.  Fork [this repo](https://github.com/prioritizr/wdpar) and clone it
    to your computer. To learn more about this process, see [this
    guide](https://guides.github.com/activities/forking/).
2.  If you have forked and cloned the project before and it has been a
    while since you worked on it, [pull changes from the original
    repo](https://help.github.com/articles/merging-an-upstream-repository-into-your-fork/)
    to your clone by using `git pull upstream master`.
3.  Open the RStudio project file (`.Rproj`).
4.  Make your changes:
    - Write your code.
    - Test your code (bonus points for adding unit tests).
    - Document your code (see function documentation above).
    - Check your code with
      [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
      and aim for 0 errors and warnings.
5.  Commit and push your changes.
6.  Submit a [pull
    request](https://guides.github.com/activities/forking/#making-a-pull-request).
