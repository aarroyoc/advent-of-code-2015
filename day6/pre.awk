BEGIN {
    print "("
}

match($0, /turn on ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)/, a) {
    print "(:turn-on " a[1] " " a[2] " " a[3] " " a[4] ")"
}
match($0, /turn off ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)/, a) {
    print "(:turn-off " a[1] " " a[2] " " a[3] " " a[4] ")"
}
match($0, /toggle ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)/, a) {
    print "(:toggle " a[1] " " a[2] " " a[3] " " a[4] ")"
}

END {
    print ")"
}
