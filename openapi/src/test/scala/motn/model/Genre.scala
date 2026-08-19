// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * Genres are used to categorize shows.
  * Each genre object contains the id and name of the genre.
  * When fetching genres via [/genres](#get-all-genres) endpoint, you can pass the output_language parameter
  * to get the genre names in one of the supported languages.
  * 
  * You can use genre ids to filter shows in the search endpoints.
  */
case class Genre(id: String,
                 name: String) derives RW
