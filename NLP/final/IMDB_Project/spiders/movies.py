# -*- coding: utf-8 -*-
from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor

SEARCH_QUERY = (
    'https://www.imdb.com/search/title?'
    'title_type=feature&'
    'genre=comedy&'
    'year=2018,2019&'
    'languages=en&'
    'count=1&'
    'view=simple'
)

class MovieSpider(CrawlSpider):
    name = 'movies'
    allowed_domains = ['imdb.com']
    start_urls = [SEARCH_QUERY]

    rules = (Rule(
        LinkExtractor(restrict_css=('div.desc a')),
        follow=True,
        callback='parse_query_page',
    ),)

    def parse_query_page(self, response):
        links = response.css('span.lister-item-header a::attr(href)').extract()
        for link in links:
            yield response.follow(link, callback=self.parse_movie_detail_page)

    def parse_movie_detail_page(self, response):
        data = {}
        data['title'] = response.css('h1::text').extract_first().strip()
        data['year'] = response.css('#titleYear a::text').extract_first()
        genres = response.xpath("//div[contains(.//h4, 'Genres')]/a/text()").extract()
        data['genre'] = [genre.strip() for genre in genres]
        data['users_rating'] = response.xpath('//span[contains(@itemprop, "ratingValue")]/text()').extract_first()
        data['imdb_url'] = response.url.replace('?ref_=adv_li_tt', '')
        yield data

