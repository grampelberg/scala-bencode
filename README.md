Scala-Bencode
=============

A simple library that I wrote to parse and generate bencoded data. This uses
the really awsome scala combinator parsing libraries and is supposedly stream
based. All the rep* methods are based off while loops instead of recursion to
make sure the recursion limit of the JVM isn't reached.

To use the decoder:

import org.saunter.bencode._

// Use your input library of choice.
import scalax.io._

val my_obj = BencodeDecoder.decode(
    InputStreamResource.file("test.torrent").reader.slurp)

For the encoder:

import org.saunter.bencode._

val encoded_str = BencodeEncoder.encode(List("foo", 10))
