# humilia

> *humilia, Latin substantive adjective* - things little or lowly

---

**humilia** is a collection of generally useful functions, much along the lines of [plumbing](https://github.com/plumatic/plumbing), [flatland/useful](https://github.com/flatland/useful), or [medley](https://github.com/weavejester/medley). This library contains code I've gradually collected over the last six years, and is a fork of [utiliva](https://github.com/Workiva/utiliva) (which I  no longer maintain).

## Rationale

This library is a loose collection of little functions that have proven themselves useful to both my personal and professional projects going back six years. That's all.

### Interacting with Maps

If you import libraries to interact with maps (`map-keys`, `map-vals`, etc.), then you may find the variants in this library to be particularly useful. The versions in this library remain consistent with the semantics of Clojure idioms, returning lazy sequences but also providing efficient transducer forms. This means that you can chain `map-keys`, `keep-vals`, `filter-keys`, etc. without the rather egregious performance hit of building up persistent maps in between each step in the chain.

## License

Copyright © 2012-2019 Timothy Dean
Copyright © 2017-2018 Workiva Inc.

Licensed under the Eclipse Public License 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

     http://opensource.org/licenses/eclipse-1.0.php

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
