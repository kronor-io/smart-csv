BEGIN {
    package_options = ""
    global_options = ""
    in_package = 0
    in_program = 0
    is_local = 0;
    if (suffix == "")
        suffix = " "
}

/^-- Kronor packages*$/ {
    c=1;
    next
}

c && /^packages:/ {
    p=1;
    next
}

p && /^[[:blank:]]+/ {
    sub(/.*\//, "", $1);
    packages[$1]["local"] = "true"
    next
}

p = 0;

/^program-options[[:blank:]]*$/ {
    in_program = 1;
    next
}

$0 ~ ("^package " ".*" "[[:blank:]]*$") {
    if (! ($2 in packages)) {
        packages[$2]["local"] = 0
    }
    in_package = $2;
    next
}

(in_package || in_program) && /^[[:blank:]]*ghc-options:/ {
    # p=1;
    if (in_package)
        in_package_ghc_options = 1;
    else if (in_program)
        in_program_ghc_options = 1;
    # indent=match($0, /[^[:blank:]]/)-1;
    next
}

(in_package_ghc_options || in_program_ghc_options) && /^[[:blank:]]/ {
    if (!need_all && match($0, /^[[:blank:]]*\+|^[[:blank:]]*-fprefer-byte-code/)) {
    } else {
        gsub(/^[[:blank:]]+|[[:blank:]]+$/, "");
        if (in_package_ghc_options) {
            package_options = package_options "\"" (prefix $0) "\"" suffix;
        } else if (in_program_ghc_options) {
            program_options = program_options "\"" (prefix $0) "\"" suffix;
        }
    }
    next
}

(in_package || in_program) {
    if (in_package) {
        packages[in_package]["ghc_options"] = package_options
        in_package = 0;
        in_package_ghc_options = 0;
        package_options = ""
    } else if (in_program) {
        in_program = 0;
        in_program_ghc_options = 0;
    }
}

END {
    print "{";
    for (package in packages) {
        ghc_options = "";

        local = "true";
        if (! (packages[package]["local"])) {
            local = "false";
        } else {
            ghc_options = program_options
        }

        if ("ghc_options" in packages[package]) {
            ghc_options = ghc_options packages[package]["ghc_options"];
        }

        printf "\"%s\" = {\"local\" = %s;\"ghc_options\" = [%s];};\n", package, local, ghc_options;
    }
    print "}"
}
