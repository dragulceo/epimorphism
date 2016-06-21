#!/usr/bin/env bash

#scp -i ~/.ssh/epimorphism.pem -r html/* ubuntu@52.40.74.208:/home/ubuntu/www/epimorphism
scp -i ~/.ssh/epimorphism.pem html/* ubuntu@52.40.74.208:/home/ubuntu/www/epimorphism
scp -i ~/.ssh/epimorphism.pem html/lib/*lib ubuntu@52.40.74.208:/home/ubuntu/www/epimorphism
