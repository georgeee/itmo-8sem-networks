Example
> stack ghci
>> let e = env ["m1", "m2", "s1"] [["m1", "m2"], ["m1", "s1"]]
>> save $ LaunchScript e
or 
>> saveTo dir $ LaunchScript e

To generate to specified file
>> writeFile "launch2.sh" $ show e

For generating all scripts with single shot, see "generate" and "generateTo" in Gen module

See also function "extEnv"
