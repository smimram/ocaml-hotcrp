ocaml-hotcrp
============

[HotCRP](https://hotcrp.com/) is an open-source online platform for managing reviews for conferences. This library allows to use its [API](https://hotcrp.com/devel/api/) in order to automate part the processing of reviews.

We provide three interface:

- the one in the `JSON` module is the low-level one, returning JSON data as-is from the server
- the toplevel one is a high-level one returning structured data (records, etc.)
- the one in `Blocking` is the same as the high-level one excepting that it is blocking and does not require dealing with Lwt
