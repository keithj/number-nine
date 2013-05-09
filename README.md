
# Synopsis #

`No. 9` (alternatively, `number-nine` or `n9`) [^1] is an Emacs
extension to aid the management of Scrum artifacts such as product and
sprint backlogs. The artifacts may be created, manipulated saved,
loaded and exported as
[MultiMarkdown](https://github.com/fletcher/peg-multimarkdown) format
reports.

# Status #

`No. 9` is brand new and a work in progress.

`No. 9`'s development is managed using `No. 9`. The current product
backlog is saved as `products/number-nine/product.el`.


All stories are
exported to the `products/number-nine/export/` directory as
[MultiMarkdown](https://github.com/fletcher/peg-multimarkdown) format.


# Usage #

#### Load No. 9's own product backlog ####

    M-x load-n9-product

#### Create a new product ####

    M-x new-n9-product

#### Add a new story to the current product ####

    M-x add-n9-story

#### Save the current product ####

    M-x save-n9-product


[^1]: *"Scrum half: A player nominated to throw the ball into a scrum who usually wears jersey No. 9."* - Laws of the Game Rugby Union.
