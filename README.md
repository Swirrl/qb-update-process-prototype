# QB Update Process Prototype

A prototype frontend cljs application to demonstrate the potential of
our proposed data cube update process.

We represent changes as a transaction log of appends and deletes, and
demonstrate how a tool can when given an update in a tidy-data qb
format can automatically help users calculate the delta of their
changes.

## Demo

A demonstration version of this application is [deployed here](https://swirrl.github.io/qb-update-process-prototype/).

## Development

Prerequisities:

- Install [shadow-cljs](https://shadow-cljs.github.io/docs/UsersGuide.html#_installation)

Then run:

```
$ npx shadow-cljs watch frontend
```

This will serve a development build of the app on `http://localhost:8080/`, start a network REPL you can connect your clj(s) editor too, whilst also enabling file watching and live reloading on the reagent application, i.e. saving files in your editor will cause the application to be automatically updated.

Some additional shadow-cljs tooling will also be available at `http://localhost:9631/`.


## License

Copyright © 2023 - TPX Impact LTD
All rights reserved
