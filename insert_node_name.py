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

    launch_filename = sys.argv[1]
    e = xml.etree.ElementTree.parse(launch_filename).getroot()

    cache_dirs = {}
    node_names = []
    def find_all_launch_file(launch_file_path):
        # print(launch_file_path, launch_file_path in cache_dirs)
        if launch_file_path in cache_dirs:
            return
        basename = os.path.basename(launch_file_path)
        cache_dirs[launch_file_path] = None
        e = xml.etree.ElementTree.parse(launch_file_path).getroot()

        node_names.extend(map(lambda x: "{}:\t{}".format(basename, x.get('name')), e.findall('node')))
        # arg_names = {x.get('name'): x.get('default', 'value') for x in e.findall('arg')}
        # for node in e.findall('node'):
        #     node_name = node.get('name')

        #     if re.search('\$\(arg \w+\)', node_name):
        #         arg_name = re.search('(?<=\(arg )((?!\)).)*', node_name).group(0)
        #         node_name = arg_names[arg_name]
        #     node_names.append('{}: {}'.format(basename, node_name))

        for launch_path in e.findall('include'):
            file_path = launch_path.get('file')
            # print("file_path = {}".format(file_path))
            package_name_group = re.search(r'(?<=\(find )((?!\)).)*', file_path)
            package_name = package_name_group.group(0)
            find_all_launch_file(os.path.join(roslib.packages.get_pkg_dir(package_name),
                                              file_path[package_name_group.end()+2:]))

    find_all_launch_file(launch_filename)
    for node_name in node_names:
        print(node_name)

if __name__ == "__main__":
    main()
