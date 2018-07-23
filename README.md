# Elm-WebDriver

## Getting Started

To run the example tests in this repo, do the following.

1. Download
   [ChromeDriver](http://chromedriver.chromium.org/getting-started)
1. Follow the instructions in the url above to put the ChromeDriver
   binary (the file you downloaded) into your PATH (google something
   like 'how to put a file in your path' to learn more)
1. Install yarn from [here](https://yarnpkg.com/lang/en/docs/install/)
1. Clone this repo (for more info on how to clone a repo, checkout
   [Atlassian's
   tutorial](https://confluence.atlassian.com/bitbucket/clone-a-repository-223217891.html))
1. `cd` into `elm-webdriver/example`
1. Run `yarn && yarn test`
    1. If you get an error similar to `Problem with Webdriver host (Unable
        to make a connection. Is your network working?)`, you may need to
        run ChromeDriver manually. Go
        [here](http://chromedriver.chromium.org/getting-started) and find
        where it describes how to 'Start the ChromeDriver server
        separately'.
