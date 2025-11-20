import java.util.Base64
import scala.annotation.tailrec
import requests.Response
import ujson.Value

case class ArtistRef(name: String, id: String)
case class TrackInfo(name: String, durationMs: Long, artists: List[ArtistRef])
case class ArtistInfo(id: String, name: String, followers: Long)

object SpotifyClient:
  private val tokenUrl = "https://accounts.spotify.com/api/token"
  private val apiBase = "https://api.spotify.com/v1"

  def readEnv(name: String): Option[String] =
    Option(System.getenv(name)).filter(_.nonEmpty)

  def getClientCredentials: Either[String, (String, String)] =
    (readEnv("SPOTIFY_CLIENT_ID"), readEnv("SPOTIFY_CLIENT_SECRET")) match
      case (Some(id), Some(secret)) => Right((id, secret))
      case _ => Left("Set SPOTIFY_CLIENT_ID and SPOTIFY_CLIENT_SECRET as env variables.")

  // Obtain client credentials token (client credentials flow)
  def getAccessToken(clientId: String, clientSecret: String): Either[String, String] =
    val creds = Base64.getEncoder.encodeToString(s"$clientId:$clientSecret".getBytes("utf-8"))
    try
      val resp = requests.post(
        url = tokenUrl,
        headers = Map("Authorization" -> s"Basic $creds"),
        data = Map("grant_type" -> "client_credentials")
      )
      if resp.statusCode == 200 then
        val json = ujson.read(resp.text())
        Right(json("access_token").str)
      else Left(s"Failed to get token: ${resp.statusCode} ${resp.statusMessage}")
    catch
      case e: Throwable => Left(s"Exception getting token: ${e.getMessage}")

  // Helper to GET with Bearer token
  private def getWithBearer(url: String, token: String): Either[String, Response] =
    try
      val r = requests.get(url, headers = Map("Authorization" -> s"Bearer $token"))
      if r.statusCode / 100 == 2 then Right(r) else Left(s"HTTP ${r.statusCode}: ${r.text()}")
    catch
      case e: Throwable => Left(s"Request failed: ${e.getMessage}")

  // Fetch all playlist tracks (handles pagination)
  def fetchAllTracks(playlistId: String, token: String): Either[String, List[TrackInfo]] =
    val first = s"$apiBase/playlists/$playlistId/tracks?limit=100"

    @tailrec
    def loop(nextUrlOpt: Option[String], acc: List[TrackInfo]): Either[String, List[TrackInfo]] =
      nextUrlOpt match
        case None => Right(acc.reverse)
        case Some(url) =>
          getWithBearer(url, token) match
            case Left(err) => Left(err)
            case Right(resp) =>
              val json = ujson.read(resp.text())
              // items -> each item has "track" object; skip null tracks
              val items = json("items").arr.toList
              val parsed = items.flatMap { item =>
                val tr = item.obj.get("track")
                tr.flatMap {
                  case ujson.Null => None
                  case t: ujson.Value =>
                    try
                      val name = t("name").str
                      val duration = t("duration_ms").num.toLong
                      val artists = t("artists").arr.toList.map { a =>
                        ArtistRef(a("name").str, a("id").str)
                      }
                      Some(TrackInfo(name, duration, artists))
                    catch
                      case _: Throwable => None
                }
              }
              val nextUrl = json.obj.get("next").flatMap{
                case ujson.Null => None
                case v => Some(v.str)
              }
              loop(nextUrl, parsed ::: acc)

    loop(Some(first), List.empty)

  // Fetch artist info for a single artist id
  def fetchArtist(artistId: String, token: String): Either[String, ArtistInfo] =
    val url = s"$apiBase/artists/$artistId"
    getWithBearer(url, token).flatMap { resp =>
      try
        val json = ujson.read(resp.text())
        val name = json("name").str
        val followers = json("followers")("total").num.toLong
        Right(ArtistInfo(artistId, name, followers))
      catch
        case e: Throwable => Left(s"Failed to parse artist $artistId: ${e.getMessage}")
    }

@main def run(): Unit =
  val playlistId = "5Rrf7mqN8uus2AaQQQNdc1"

  val result = for
    creds <- SpotifyClient.getClientCredentials
    (clientId, clientSecret) = creds
    token  <- SpotifyClient.getAccessToken(clientId, clientSecret)
    tracks <- SpotifyClient.fetchAllTracks(playlistId, token)
  yield (token, tracks)

  result match
    case Left(err) =>
      Console.err.println(s"Error initializing: $err")
    case Right((token, tracks)) =>
      // Part 1: top 10 longest tracks
      val top10 = tracks.sortBy(- _.durationMs).take(10)

      println("PART 1: Top 10 longest songs (SongName , duration_ms)\n")
      top10.foreach { t =>
        println(s"${t.name} , ${t.durationMs}")
      }

      // get all artist IDs from top10 (unique)
      val artistIds = top10.flatMap(_.artists.map(_.id)).filter(_ != "null").distinct

      // For each artist id, fetch artist info
      val artistsEitherList = artistIds.map(id => SpotifyClient.fetchArtist(id, token))
      // combine results: collect errors or continue with successful ones
      val (errors, artists) = artistsEitherList.foldLeft((List.empty[String], List.empty[ArtistInfo])) {
        case ((errs, acc), either) =>
          either match
            case Left(e)  => (e :: errs, acc)
            case Right(a) => (errs, a :: acc)
      }

      // notify if there were errors fetching some artists
      if errors.nonEmpty then
        Console.err.println("Warnings: some artist fetches failed:")
        errors.reverse.foreach(e => Console.err.println(s" - $e"))

      // Part 2: print artists ordered by followers descending
      val sortedArtists = artists.sortBy(- _.followers)
      println("\nPART 2: Artists ordered by follower count (Artist : follower_count)\n")
      sortedArtists.foreach { a =>
        println(s"${a.name} : ${a.followers}")
      }
