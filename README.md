# dict
An ISO Prolog dictionary, implemented as an incomplete binary search tree.

Inspired by Chapter 15.3 from [The Art of Prolog](https://mitpress.mit.edu/9780262691635/the-art-of-prolog/)  by Sterling and Shapiro.

Tested with [Scryer Prolog](https://scryer.pl).

## Exports

* `add(+Key, ?Dict, ?Value)` - adds a key value pair to the dictionary, if it doesn't already exist.
* `balance(+Dict, -Dict)` - balances a dictionary.
* `dict_list(?Dict, ?List)` - serializes a dictionary to a list of key value pairs, or vice-versa.
* `get(+Key, +Dict, -Value)` - gets the value associated with the key. Fails if key does not exist.
* `put(+Key, ?Dict, -Dict1, +Value)` - adds/updates the value of a key, returning a new dictionary.

## Testing

    $ scryer-prolog -f test/dict.pl
    Running test "add/get"
    Running test "put"
    Running test "dict_list"
    Running test "balance"
