while (<>) {
    print if /^((?!\s).+(?<!\s)|)\R?$/
}
