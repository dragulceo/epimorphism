#!/usr/bin/env bash

scp -i ~/.ssh/epimorphism.pem html/* ubuntu@52.40.74.208:/home/ubuntu/www/epimorphism
scp -i ~/.ssh/epimorphism.pem -r html/javascript* ubuntu@52.40.74.208:/home/ubuntu/www/epimorphism
scp -i ~/.ssh/epimorphism.pem -r html/stylesheets* ubuntu@52.40.74.208:/home/ubuntu/www/epimorphism
scp -i ~/.ssh/epimorphism.pem lib/* ubuntu@52.40.74.208:/home/ubuntu/www/epimorphism/lib
scp -i ~/.ssh/epimorphism.pem -r html/misc* ubuntu@52.40.74.208:/home/ubuntu/www/epimorphism
scp -i ~/.ssh/epimorphism.pem -r html/images* ubuntu@52.40.74.208:/home/ubuntu/www/epimorphism
#scp -i ~/.ssh/epimorphism.pem -r html/textures ubuntu@52.40.74.208:/home/ubuntu/www/epimorphism
