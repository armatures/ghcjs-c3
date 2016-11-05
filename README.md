# ghcjs-c3

Simple [C3](http://c3js.org/) bindings for GHCJS. This is just a bare minimum starting point for further development.

To view some sample charts, run `app/Main.hs`:

```
git clone https://github.com/mbeidler/ghcjs-c3
cd ghcjs-c3
stack setup
stack build
npm install && npm run go
open index.html
```

## TODO

- Type-safe interface
- Use functions to build configuration instead of data constructors.
- Support more chart options
- Timeseries charts with `Day` values on x-axis
- Support any numeric type as well as generic containers.

## Contributing

If you'd like to contribute to this project, don't hesitate to submit pull requests!
