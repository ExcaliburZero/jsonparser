# Jsonparser [![Travis CI Status](https://api.travis-ci.org/ExcaliburZero/jsonparser.svg)](https://travis-ci.org/ExcaliburZero/jsonparser) [![Coverage Status](https://coveralls.io/repos/github/ExcaliburZero/jsonparser/badge.svg?branch=master)](https://coveralls.io/github/ExcaliburZero/jsonparser?branch=master)
Jsonparser is an experimental Haskell library for parsing JSON.

```haskell
>>> parse valueJSON "" "{\"a\":true}"
Right (JSONObject [("a", JSONBool True)])
```

## License
The source code of Jsonparser is available under the [MIT license](https://opensource.org/licenses/MIT), see `LICENSE` for more information.
