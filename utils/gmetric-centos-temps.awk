#!/usr/bin/awk -f

# Extract system temperatures from lm_sensors and
# feed them into Ganglia gmond by calling gmetric.

BEGIN {
    gmetric = "/usr/bin/gmetric";
    unit = "none";
}

/^radeon-pci-/ {
    unit = "GPU";
}

/^k10temp-pci-/ {
    unit = "CPU";
}

/^f71889ed-isa-/ {
    unit = "SYS";
}

/^temp[0-9]:/ {
    number = substr($1, 5, 1);
    match($2, /+([0-9.]+)°C/, matches);
    temp = substr($2, matches[1, "start"], matches[1, "length"]);
    temps[unit "_" number] = temp;
}

END {
    for (temp in temps) {
        system(gmetric " -n " temp " -v " temps[temp] " -t uint16 -u Celsius");
    }
}
