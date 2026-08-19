// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * A show object represents a movie or a series. Type of the show is determined by the showType property,
  * which is either movie or series. Based on this type, some properties are omitted,
  * for example a movie does not have seasonCount and episodeCount properties.
  * 
  * Show object contains the details such as the title, overview, genres, cast, rating and images.
  * You can find the streaming availability information under streamingOptions property.
  * Each streaming option contains the service info, deep link, video quality, available audios and subtitles
  * and more. It also includes the price if the show is available to buy or rent;
  * and addon info if the show is available via an addon (such as Apple TV Channels, Prime Video Channels etc.).
  * 
  * You can also find the seasons of the series under the seasons property,
  * and the episodes of a season under the episodes property of the season object.
  * Via the streamingOptions property of seasons and episodes,
  * you can get the individual streaming options of them.
  * These streaming options include the same set of properties as the show streaming options,
  * so you can use them to get deep links to episodes and seasons, and to see available audios and subtitles.
  * 
  * Note that seasons and episodes are not included in the search results
  * unless you set the series_granularity parameter to seasons or episodes.
  * For more info, check out the series_granularity parameter of the search endpoints.
  * 
  * There are multiple ways to retrieve shows.
  * You can retrieve a show by its IMDb or TMDB id via [/shows/{id}](#get-a-show) endpoint;
  * you can search shows by their title via [/shows/search/title](#search-shows-by-title) endpoint;
  * and you can search shows by filters such as genres, release year, rating etc.
  * via [/shows/search/filters](#search-shows-by-filters) endpoint. This endpoint also supports pagination
  * and offers advanced ordering options such as ordering by popularity, rating, release year etc.
  */
case class Show(itemType: String,
                 showType: ShowType,
                 id: String,
                 imdbId: String,
                 tmdbId: String,
                 title: String,
                 overview: String,
                 originalTitle: String,
                 genres: List[Genre],
                 cast: List[String],
                 rating: Int,
                 imageSet: ShowImageSet,
                 streamingOptions: Map[String, List[StreamingOption]],
                 releaseYear: Option[Int] = None,
                 firstAirYear: Option[Int] = None,
                 lastAirYear: Option[Int] = None,
                 directors: List[String] = Nil,
                 creators: List[String] = Nil,
                 seasonCount: Option[Int] = None,
                 episodeCount: Option[Int] = None,
                 runtime: Option[Int] = None,
                 seasons: List[Season] = Nil) derives RW
