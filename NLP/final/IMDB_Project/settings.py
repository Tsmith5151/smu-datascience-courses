# -*- coding: utf-8 -*-

# Scrapy settings for IMDB_Project project
#
# For simplicity, this file contains only settings considered important or
# commonly used. You can find more settings consulting the documentation:
#
#     https://doc.scrapy.org/en/latest/topics/settings.html
#     https://doc.scrapy.org/en/latest/topics/downloader-middleware.html
#     https://doc.scrapy.org/en/latest/topics/spider-middleware.html

BOT_NAME = 'IMDB_Project'

SPIDER_MODULES = ['IMDB_Project.spiders']
NEWSPIDER_MODULE = 'IMDB_Project.spiders'

# Crawl responsibly by identifying yourself (and your website) on the user-agent
#USER_AGENT = 'IMDB_Project (+http://www.yourdomain.com)'

# Obey robots.txt rules
ROBOTSTXT_OBEY = True

# Maximum Items Returned 
CLOSESPIDER_ITEMCOUNT = 10

# See https://doc.scrapy.org/en/latest/topics/settings.html#download-delay
# See also autothrottle settings and docs
DOWNLOAD_DELAY = 3

# Saving the output in json format
FEED_URI = 'IMDB_Project/data/%(name)s.json'
FEED_FORMAT = 'json'