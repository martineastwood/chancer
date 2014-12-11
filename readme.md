## Chancer - Creating Random Data in R

Chancer is an R package for generating of random strings, numbers, addresses etc to help write automated tests or create anything random you need. Chancer was inspired by the javascript library [chance.js](http://chancejs.com).

### Installation
Chancer is not currently on CRAN as it's still in early development but in the meantime it can be installed in R using devtools

```R
install_github("martineastwood/chancer")
```

### Examples

Take a look at the documentation to see all the functions, but there are a few brief examples below to give you an idea about chancer:

`chancer.word`: Returns a random semi-pronounceable word with control over number of syllables etc.

`chancer.sentence`: Returns a random sentence constructed from semi-pronounceable made up words.

`chancer.paragraph`: Returns a random paragraph constructed from semi-pronounceable made up sentances.

`chancer.address`: Returns a random address constructed from semi-pronounceable made up words.

`chancer.name`: Returns a random name, with control over gender, prefixs such as Doctor etc.

`chancer.birthday`: Returns a random birthday, with contol of overgroups e.g child, adult etc.

`chancer.colour`: Returns a random rgb / hex / greyscale colour.

`chancer.domain`: Returns a random semi-pronounceable domain name.

`chancer.email`: Returns a random semi-pronounceable email address.

`chancer.ip`: Returns a random ip address.

`chancer.ssn`: Returns a random US social security number.

`chancer.guid`: Returns a random guid.

`chancer.geohash`: Returns a random geohash.

`chancer.coordinates`: Returns random coordinates.

`chancer.city`: Returns a random made up city.

`chancer.areacode`: Returns a random area code.

There are lots of others already included in the package, with more to follow! 