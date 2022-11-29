#!/bin/bash
sed -i 's/^ *# *maintenance_work_mem *= *[^ ]*/maintenance_work_mem = 1GB #/' /var/lib/postgresql/data/postgresql.conf
sed -i 's/^ *# *max_parallel_maintenance_workers *= *[^ ]*/max_parallel_maintenance_workers = 4 #/' /var/lib/postgresql/data/postgresql.conf
