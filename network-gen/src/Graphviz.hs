module Graphviz where

class Graphviz g where
    buildGraph :: g -> String

