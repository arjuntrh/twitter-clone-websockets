
open Suave
open Suave.Http
open Suave.Operators
open Suave.Filters
open Suave.Successful
open Suave.Files
open Suave.RequestErrors
open Suave.Logging
open Suave.Utils

open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open Newtonsoft.Json

open System
open System.Net
open System.Text.Json
open System.Collections.Generic

// Request, Response classes for Json serialization and deserialization
type User =
    {
        FirstName : string
        LastName : string
    }

type RequestObj =
    {
        Task: string
        UserID: string
        Password: string
        TargetUserID: string
        Tweet: string
        Hashtag: string
        Mention: string
    }

type ResponseObj (responseTask: string, responseStatus:string, responseMessage: string, responseData: List<string>) = 
  member this.ResponseTask = responseTask
  member this.ResponseStatus = responseStatus
  member this.ResponseMessage = responseMessage
  member this.ResponseData = responseData

// default response values
let _responseTask = ""
let _responseStatus = ""
let _responseMessage = ""
let _responseData = new List<string>()

// Database
let usersTable     = new Dictionary<string, string>()
let loggedIn     = new Dictionary<string, bool>()
let followersTable = new Dictionary<string, List<string>>()
let followingTable = new Dictionary<string, List<string>>()
let newsFeedTable  = new Dictionary<string, List<string>>()
let hashtagsTable  = new Dictionary<string, List<string>>()
let mentionsTable  = new Dictionary<string, List<string>>()
let webSocketMap =  new Dictionary<string, WebSocket>()

// Helpers---------------------------------------------------------------------
let isUserRegistered(userID) = 
  usersTable.ContainsKey(userID)

let isUserLoggedIn(userID) = 
  loggedIn.[userID]

let isValidPassword(userID, givenPassword) = 
  let actualPassword = usersTable.[userID]

  actualPassword = givenPassword

let processTweet(tweet:string) = 
  let mutable mention = ""
  let mutable hashtag = ""
  let hashtagList = new List<string>()
  let mentionList = new List<string>()

  let mutable i = 0;

  while i < tweet.Length do
    if tweet.[i] = '#' then
        while i < tweet.Length && tweet.[i] <> ' ' do
            hashtag <- hashtag + (tweet.[i] |> string)
            i <- i + 1

        if hashtag.Length > 1 then
          hashtagList.Add(hashtag)

        hashtag <- ""

    elif tweet.[i] = '@' then
        while i < tweet.Length && tweet.[i] <> ' ' do
            mention <- mention + (tweet.[i] |> string)
            i <- i + 1

        if mention.Length > 1 then
          mentionList.Add(mention)
          
        mention <- ""

    i <- i + 1
  
  for hashtag in hashtagList do
    if hashtagsTable.ContainsKey(hashtag) then
        hashtagsTable.[hashtag].Add(tweet)
    else
        hashtagsTable.Add(hashtag, new List<(string)>())
        hashtagsTable.[hashtag].Add(tweet)

  for mention in mentionList do
    if mentionsTable.ContainsKey(mention) then
        mentionsTable.[mention].Add(tweet)
    else
        mentionsTable.Add(mention, new List<(string)>())
        mentionsTable.[mention].Add(tweet)    


let FetchFeedHelper (userID) =
  if (not (isUserLoggedIn(userID))) then
    let myObj = ResponseObj("SendTweet", "0", ". You have not logged in. Please login first to see your feed", _responseData)
    myObj
  else
    let newsFeed = newsFeedTable.[userID]

    let myObj = ResponseObj("SendTweet", "1", "News Feed Updated", newsFeed)
    myObj

let getByteResponse(response: ResponseObj) = 
  // convert object to JSON string first
  let responseJsonStr = JsonSerializer.Serialize response

  // the response needs to be converted to a ByteSegment
  let byteResponse =
    responseJsonStr
    |> System.Text.Encoding.ASCII.GetBytes
    |> ByteSegment

  byteResponse

// APIs------------------------------------------------------------------------
let RegisterUser (jsonRequestObj) =
  printfn "RegisterUser API called"

  let userID = jsonRequestObj.UserID
  let password = jsonRequestObj.Password

  // add a new user only if the user doesnt exit yet
  if isUserRegistered(userID) then
    let myObj = ResponseObj(jsonRequestObj.Task, "0", "User already exists. Please register with a different name", _responseData)
    myObj
  else
      usersTable.Add(userID, password) 
      loggedIn.Add(userID, true) 
      newsFeedTable.Add(userID, new List<string>())
      followersTable.Add(userID, new List<string>())
      followingTable.Add(userID, new List<string>())

      let myObj = ResponseObj(jsonRequestObj.Task, "1", "Registration Successful", _responseData)
      myObj

let LoginUser (jsonRequestObj) =
  printfn "LoginUser API called"

  let userID = jsonRequestObj.UserID
  let password = jsonRequestObj.Password

  if not (isUserRegistered(userID)) then
    let myObj = ResponseObj(jsonRequestObj.Task, "0", "User not registered", _responseData)
    myObj
  else
    if isValidPassword(userID, password) then
      loggedIn.[userID] <- true

      let myObj = ResponseObj(jsonRequestObj.Task, "1", "Login Successful", _responseData)
      myObj
    else
      let myObj = ResponseObj(jsonRequestObj.Task, "0", "Invalid password. Please enter the correct password!", _responseData)
      myObj

let LogoutUser (jsonRequestObj) =
  printfn "LogoutUser API called"

  let userID = jsonRequestObj.UserID

  if not (loggedIn.[userID]) then
    let myObj = ResponseObj(jsonRequestObj.Task, "0", "You have logged out", _responseData)
    myObj
  else
    loggedIn.[userID] <- false
    let myObj = ResponseObj(jsonRequestObj.Task, "1", "Logout Successful", _responseData)
    myObj

let Subscribe (jsonRequestObj) =
  printfn "Subscribe API called"

  let userID = jsonRequestObj.UserID
  let targetUserID = jsonRequestObj.TargetUserID

  if not (isUserRegistered(targetUserID)) then
    let myObj = ResponseObj(jsonRequestObj.Task, "0", "Target user not found", _responseData)
    myObj
  else
    followersTable.[targetUserID].Add(userID)
    followingTable.[userID].Add(targetUserID)

    let responseMsg = "You are now following " + targetUserID
    let followingList = followingTable.[userID]

    let myObj = ResponseObj(jsonRequestObj.Task, "1", responseMsg, followingList)
    myObj

let FetchFeed (jsonRequestObj) =
  printfn "FetchFeed API called"

  let userID = jsonRequestObj.UserID
  let newsFeed = newsFeedTable.[userID]

  let myObj = ResponseObj(jsonRequestObj.Task, "1", "News Feed returned Successfully", newsFeed)
  myObj

let FetchFollowers (jsonRequestObj) =
  printfn "FetchFollowers API called"

  let userID = jsonRequestObj.UserID
  let followersList = followersTable.[userID]

  let myObj = ResponseObj(jsonRequestObj.Task, "1", "Followers list returned Successfully", followersList)
  myObj

let FetchFollowing (jsonRequestObj) =
  printfn "FetchFollowing API called"

  let userID = jsonRequestObj.UserID
  let followingList = followingTable.[userID]

  let myObj = ResponseObj(jsonRequestObj.Task, "1", "Following list returned Successfully", followingList)
  myObj

let SendTweet (jsonRequestObj) =
  printfn "SendTweet API called"

  let userID = jsonRequestObj.UserID
  let tweet = jsonRequestObj.Tweet

  if (not (isUserLoggedIn(userID))) then
    let myObj = ResponseObj(jsonRequestObj.Task, "0", "Please login first", _responseData)
    myObj
  else
    let tweetActual = userID + ": " + tweet
    processTweet(tweetActual)

    newsFeedTable.[userID].Add(tweetActual)

    let followersList = followersTable.[userID]
    for user in followersList do
        newsFeedTable.[user].Add(tweetActual)

    let myFeed = newsFeedTable.[userID]
    let myObj = ResponseObj(jsonRequestObj.Task, "1", "Sent tweet successfully", myFeed)
    myObj


let Retweet (jsonRequestObj) = 
  printfn "Retweet API called"

  let userID = jsonRequestObj.UserID
  let tweet = jsonRequestObj.Tweet

  let retweet = tweet + "-- Retweeted by " + userID
  newsFeedTable.[userID].Add(retweet)

  let followersList = followersTable.[userID]
  for user in followersList do
      newsFeedTable.[user].Add(retweet)

  let myFeed = newsFeedTable.[userID]
  let myObj = ResponseObj(jsonRequestObj.Task, "1", "Retweeted successfully", myFeed)

  myObj

let QueryHashtag (jsonRequestObj) = 
  printfn "QueryHashtag API called"

  let hashtag = jsonRequestObj.Hashtag

  if (hashtagsTable.ContainsKey(hashtag)) then
    let queryResult = hashtagsTable.[hashtag]
    let myObj = ResponseObj(jsonRequestObj.Task, "1", "Query Hashtag Successful", queryResult)
    myObj
  else
    let myObj = ResponseObj(jsonRequestObj.Task, "0", "No tweets found for that Hashtag!", _responseData)
    myObj

let QueryMention (jsonRequestObj) =
  printfn "QueryMention API called"

  let mention = jsonRequestObj.Mention

  if (mentionsTable.ContainsKey(mention)) then
    let queryResult = mentionsTable.[mention]
    let myObj = ResponseObj(jsonRequestObj.Task, "1", "Query Mention Successful", queryResult)
    myObj
  else
    let myObj = ResponseObj(jsonRequestObj.Task, "0", "No tweets found for that Mention!", _responseData)
    myObj

//Web Socket handler---------------------------------------------------------------------
let ws (webSocket : WebSocket) (context: HttpContext) =
  socket {
    // if `loop` is set to false, the server will stop receiving messages
    let mutable loop = true

    while loop do
      // the server will wait for a message to be received without blocking the thread
      let! msg = webSocket.read()

      match msg with
      | (Text, data, true) ->        
        let str = UTF8.toString data
        let myRequestObj = JsonConvert.DeserializeObject<RequestObj>(str)
        let mutable response = ResponseObj(_responseTask, _responseStatus, _responseMessage, _responseData)

        if not (webSocketMap.ContainsKey(myRequestObj.UserID)) then
          webSocketMap.[myRequestObj.UserID] <- webSocket

        match myRequestObj.Task with
        | "RegisterUser" -> response <- RegisterUser(myRequestObj)
        | "LoginUser" -> response <- LoginUser(myRequestObj)
        | "Subscribe" -> response <- Subscribe(myRequestObj)
        | "FetchFeed" -> response <- FetchFeed(myRequestObj)
        | "FetchFollowers" -> response <- FetchFollowers(myRequestObj)
        | "FetchFollowing" -> response <- FetchFollowing(myRequestObj)
        | "SendTweet" -> response <- SendTweet(myRequestObj)
        | "Retweet" -> response <- Retweet(myRequestObj)
        | "QueryHashtag" -> response <- QueryHashtag(myRequestObj)
        | "QueryMention" -> response <- QueryMention(myRequestObj)
        | "LogoutUser" -> response <- LogoutUser(myRequestObj)
        |_ -> printfn "Unmatched"

        if (myRequestObj.Task = "Subscribe" && response.ResponseStatus = "1") then
          let targetUserFollowersList = followersTable.[myRequestObj.TargetUserID]
          let responseMessage = myRequestObj.UserID + " followed you"
          let targetUserResponse = ResponseObj("Followers", "1", responseMessage, targetUserFollowersList)
          let byteResponse = getByteResponse(targetUserResponse)
          do! webSocketMap.[myRequestObj.TargetUserID].send Text byteResponse true

        if ((myRequestObj.Task = "SendTweet" || myRequestObj.Task = "Retweet") && response.ResponseStatus = "1") then
          // multicast
          let followersList = followersTable.[myRequestObj.UserID]
          let mutable feedResponse = ResponseObj(_responseTask, _responseStatus, _responseMessage, _responseData)

          for user in followersList do
            // get user's websocket from map and send the tweet
            feedResponse <- FetchFeedHelper(user)

            if (feedResponse.ResponseStatus = "1") then
              let byteResponse = getByteResponse(feedResponse)

              // the `send` function sends a message back to the client
              do! webSocketMap.[user].send Text byteResponse true
        
        // unicast
        let byteResponse = getByteResponse(response)
        do! webSocket.send Text byteResponse true

      | (Close, _, _) ->
        let emptyResponse = [||] |> ByteSegment
        do! webSocket.send Close emptyResponse true

        // after sending a Close message, stop the loop
        loop <- false

      | _ -> ()
    }

let app : WebPart = 
  choose [
    path "/websocket" >=> handShake ws
    GET >=> choose [ path "/twitter" >=> file "index.html"; browseHome 
                    //  path "/testroute" >=> request (fun r -> OK ("Hello from test route"))
                    ]
    NOT_FOUND "Found no handlers." ]
    
[<EntryPoint>]
let main _ =
  startWebServer { defaultConfig with logger = Targets.create Verbose [||] } app
  0

//
// The FIN byte:
//
// A single message can be sent separated by fragments. The FIN byte indicates the final fragment. Fragments
//
// As an example, this is valid code, and will send only one message to the client:
//
// do! webSocket.send Text firstPart false
// do! webSocket.send Continuation secondPart false
// do! webSocket.send Continuation thirdPart true
//
// More information on the WebSocket protocol can be found at: https://tools.ietf.org/html/rfc6455#page-34
//