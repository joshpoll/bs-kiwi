@bs.val @bs.module("fs")
external readStdInAsUtf8Sync: (@bs.as(0) _, @bs.as("utf8") _) => string = "readFileSync"