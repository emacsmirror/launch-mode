#!/usr/bin/env python
# -*- coding:utf-8 -*-

import xml.etree.ElementTree
import sys
import os
import roslib
import re

def main():
    if len(sys.argv[1:]) < 1:
        sys.exit(1)

    launch_file_path = sys.argv[1]
    package_name_group = re.search(r'(?<=\(find )((?!\)).)*', launch_file_path)
    if package_name_group is None:
        full_path = launch_file_path
    else:
        package_name = package_name_group.group(0)
        full_path = os.path.join(roslib.packages.get_pkg_dir(package_name),
                                 launch_file_path[package_name_group.end()+2:])
    print(full_path)

if __name__ == "__main__":
    main()
