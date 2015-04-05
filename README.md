# Peercoin vote

## Description

This program implements a voting system based on data from the blockchain (addresses and balances).
Given a block number (the date of the vote), each address with a positive balance can vote, and the weight of the vote is the balance of the address.

It is written in Common Lisp.

## Dependencies

* A Common Lisp implementation. Tested with:
  * [sbcl](http://www.sbcl.org)
  * [clozurecl](http://ccl.clozure.com)
  * [ecl](http://ecls.sourceforge.net)
* [cffi](http://www.cliki.net/CFFI)
* [cl-base64](http://www.cliki.net/cl-base64)
* [ironclad](http://cliki.net/Ironclad)
* [split-sequence](http://www.cliki.net/SPLIT-SEQUENCE)

## Installation

* Install [quicklisp](http://www.quicklisp.org/beta/) to manage the packages.
* Install the dependencies: ```(ql:quickload '("cffi" "cl-base64" "ironclad" "split-sequence"))```
* Copy the source code of the peercoin blockchain parser where you want it to be.
* Tell your Common Lisp implementation where to find the sources:
  * ```(push "directory-where-the-sources-are/" asdf:*central-registry*)```
  * If you don't want to type this line every time, you can add it to the initialization file (e.g.: .sbclrc, .ccl-init.lisp, .eclrc).

## Usage

First, you must prepare the data required for the vote:

* A file describing the motion the vote is about.
* A file containing the hash of the motion (that is *sha256(sha256(motion file))*). Il can be generated with the *make-motion-hash* function in the *tools* directory.
* A file containing the candidates (depending on the motion, it could be yes, no, blank, names of people, etc.).
* A file containing the Peercoin addresses and their balances at the block number chosen as the time of vote. It can be generated with the *make-balance-file* function in the *tools* directory if you have the blockchain in a database created with the [peercoin-blockchain-parser](http://github.com/glv2/peercoin-blockchain-parser) program.
* A file containing the votes.

Then, you can load the vote package and compute the results of the vote:

    (require 'peercoin-vote)
    (peercoin-vote:vote "motion-hash" "candidates.txt" "votes.txt" "balances.txt")

## File formats

### Candidate file

The candidate file must have one candidate per line.

Example:

    yes
    no
    blank

### Balance file

The balance file must have one "address balance" pair per line.
The balance must be in µPPC, in order to have only integers and avoid floating point number rounding issues.

Example (with testnet addresses):

    mm8Fwn92RU8zvJmH7TCpaYL3v4PTyjN4xd 755411090000
    mub5ke5cWP4nZW2VDgtAkFGA7UzSVhwese 70995300000
    mw2pj33HMhRfRkKtceHcyKpPiGYkPdD4SM 86083040000
    mxKnMQktAG1RcQwYQvypV4jLyjxSkYEecb 94048220000

### Vote file

The vote file must have one "address vote signature" tuple per line.
The format of the vote must be "motion-hash:candidate".
The signature must be the base64 encoded signature of the vote (just the "motion-hash:candidate" string without new line after it) by the private key associated with the address. It can be generated with the message signing feature of the Peercoin wallet.

Example (with testnet addresses):

    mw2pj33HMhRfRkKtceHcyKpPiGYkPdD4SM b883a8983f9f8776a8353d81518bc6aaed6b157f7bc0475e5b7d01ce87efb099:yes IOr3LCBd1crquXpJq9NuKBBy+F9W665rtudZkJv1LvDNwdjxuki46PLm55r+UXOs5woGg4bbvMWMwl8pUs2tqjk=
    mm8Fwn92RU8zvJmH7TCpaYL3v4PTyjN4xd b883a8983f9f8776a8353d81518bc6aaed6b157f7bc0475e5b7d01ce87efb099:yes IC5AdoS59Mtga9Oy2UJPZrVdIhbKZ2hoFkFbD6Q6cSvSY80icJwfHHIZKIkB85/Q7F+tyCZkP7YuGyjzZ5Y2ORI=
    mxKnMQktAG1RcQwYQvypV4jLyjxSkYEecb b883a8983f9f8776a8353d81518bc6aaed6b157f7bc0475e5b7d01ce87efb099:no IDSTKkxFYOfG5DdXp/8n+mL4cY7fK9+ftrWWbzxGEess4hjoFZn+vcWWFkKD00/vzZWDzTnQZsNUvRRPLO+3Sl8=
    mub5ke5cWP4nZW2VDgtAkFGA7UzSVhwese b883a8983f9f8776a8353d81518bc6aaed6b157f7bc0475e5b7d01ce87efb099:blank IITxbg1bn8PXb6SNkM+h/HjZ1I16VZlGI8CbA1nuyotKBl0K4iezqMBIagqGJncOqbOp/Smd2oPu8rH9+oqFubU= 

A vote is considered as invalid if:

* the address doesn't exist.
* the balance of the address if 0.
* the address has already voted.
* the motion hash doesn't match.
* the candidate doesn't exist.
* the signature is incorrect.

## Donations

If you find this program useful and want to make a donation, you can send coins to the following Peercoin address: PWFNV1Cvq7nQBRyRueuYzwmDNXUGpgNkBC.
