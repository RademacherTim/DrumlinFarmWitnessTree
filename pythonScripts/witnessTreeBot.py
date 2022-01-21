#========================================================================================
# This script is the twitterbot. It reads messages for a particular hour and 
# associated information to transfer it to a linked twitter account with access 
# details for the account in the config file.
# Each message can only be posted once, so accidental repetition is impossible.
#----------------------------------------------------------------------------------------

# import dependencies
#----------------------------------------------------------------------------------------
import sys        # library to use command line arguments
import tweepy     # twitter library
import twitter    # python-twitter library
import tkinter    # graphical user interface library
import facebook   # library for facebook API 
import csv        # for csv handling
import pandas     # for csv file handling
import os         # library to interact with operating system
import random     # library to use random number generator
import array      # library for array handling
import pytz       # for timezone handling
import math       # for nan handling
from datetime import date, time, datetime, timedelta

# credentials for twitter and facebook
#----------------------------------------------------------------------------------------
consumer_key        = sys.argv [1] # twitter accountconsumer key
consumer_secret     = sys.argv [2] # twitter accountconsumer secrets
access_token        = sys.argv [3] # twitter account access token
access_token_secret = sys.argv [4] # twitter account access token secret
page_access_token   = sys.argv [5] # facebook page access token
facebook_page_id    = sys.argv [6] # facebook page ID
path                = sys.argv [7] # path to the current working directory
#print (consumer_key)
#print (consumer_secret)
#print (access_token)
#print (access_token_secret)
#print (page_access_token)
#print (facebook_page_id)

# get working directory
#----------------------------------------------------------------------------------------
os.chdir (path)

# authenticate the twitter account
#----------------------------------------------------------------------------------------
auth = tweepy.OAuthHandler (consumer_key, consumer_secret)
auth.set_access_token (access_token, access_token_secret)
api = tweepy.API (auth)

# check access and create user object used later
#----------------------------------------------------------------------------------------
user = api.verify_credentials ()

# set time zones for local and twitter
#----------------------------------------------------------------------------------------
local = pytz.timezone ("US/Eastern")
localTwitter = pytz.timezone ("UTC")

# check whether there is a post file for now
#----------------------------------------------------------------------------------------
now = datetime.now ()
local_now = local.localize (now, is_dst = None)
fileName = path + "posts/%s.csv" % now.strftime ("%Y-%m-%d_%H")
print (str (now) + '; python: (2.1)  Looking for: '+ fileName)

# read in timestamp, when we last replied to interactive tweets.
#----------------------------------------------------------------------------------------
if os.path.exists (path + 'code/memory.csv'):
  tmpMem = pandas.read_csv (path + 'code/memory.csv')
  lastResponseTime = pandas.to_datetime (tmpMem ['lastResponse'] [0])
  print (str (now) + '; python: (2.2)  Read last response time from memory.')
else:
  print (str (now) + '; python: (2.2)  Error: Could not find a last response time.')

# read post, if it exists 
#----------------------------------------------------------------------------------------
if os.path.exists (fileName):
  tmp = pandas.read_csv (fileName)

  # Extract post information from the file
  #------------------------------------------------------------------------------------
  priority   = tmp ['priority']   [0]
  fFigure    = tmp ['fFigure']    [0]
  figureName = tmp ['figureName'] [0]
  message    = tmp ['message']    [0]
  hashtags   = tmp ['hashtags']   [0]
  if math.isnan (hashtags):
    post = message
  else:
    post = message + hashtags
  expires    = tmp ['expires']    [0]
  print (str (now) + '; python: (2.3)  Will post: '+ message)

  # Search for when this post was last posted
  #------------------------------------------------------------------------------------
  lastMessage = api.user_timeline (screen_name = 'awitnesstree', 
                                   include_rts = 'false', 
                                   count = 1)
  if len (lastMessage)-1 >= 0:
    lastPostTime = lastMessage [0].created_at
    oneHourFromLastPost = lastPostTime + timedelta (hours = 1)
  else: 
    oneHourFromLastPost = local_now - timedelta (hours = 1)

  if local_now.astimezone (pytz.utc) > oneHourFromLastPost.astimezone (pytz.utc):

    # get graph for facebook
    #------------------------------------------------------------------------------------
    #graph = facebook.GraphAPI (page_access_token, version = '3.1')        

    # post is accompanied by an image
    #------------------------------------------------------------------------------------
    if fFigure:
      file = api.media_upload (filename = figureName)
      tweet = api.update_status (status = post, media_ids = [file.media_id])
      #graph.put_photo (image = open (figureName, 'rb'), message = post)
      
    # post is not accompanied by an image
    #------------------------------------------------------------------------------------
    else:
      tweet = api.update_status (status = post)
      #graph.put_object (parent_object = facebook_page_id, 
      #                  connection_name = "feed", 
      #                  message = post)
      
  else:
    print (str (now) + '; python: (2.3)  Error: Last post was less than 1 hours ago!')

else:
  print (str (now) + '; python: (2.3)  Error: No file with a message!')

#----------------------------------------------------------------------------------------
# respond to various tweets
#----------------------------------------------------------------------------------------

# read in interactive responses to respond to questions
#----------------------------------------------------------------------------------------
if os.path.exists (path + 'tmp/interactiveResponses.csv'):
  responses = pandas.read_csv (path + 'tmp/interactiveResponses.csv')
  print (str (now) + '; python: (2.4)  Responses for interactive tweets read in.')
else:
  print (str (now) + '; python: (2.4)  Error: No responses for interactive messages available!')
  
# create list of tweets along the lines of how are you
#----------------------------------------------------------------------------------------
questions = ['how are you',
             'how are you doing',
             'how are you feeling',
             'how r u',
             'how are u',
             'how r u doing',
             'how do you do',
             'how\'s it going',
             'how are you doing']

# respond to tweets containing the above questions
#----------------------------------------------------------------------------------------
tweetIDs = []
responseCount = 0
for i in questions:
  tmpTweets = api.search_tweets (q = "@%s " % (user.screen_name) + i)
  previousResponses = api.user_timeline (screen_name = "awitnesstree", count = 50)
  tweets = []
	
  # get list of all tweets since last having responded
  #-------------------------------------------------------------------------------------
  for tweet in tmpTweets:
    local_dt = tweet.created_at
    questionTime = local_dt.astimezone (pytz.utc)
    if questionTime > lastResponseTime: tweets.append (tweet)	
  
  # loop over unanswered tweets
  #------------------------------------------------------------------------------------
  if tweets:
    for tweet in tweets:
      handle = tweet.user.screen_name	
      if tweet.user.screen_name != 'awitnesstree':
        for previousResponse in previousResponses:
          #print (previousResponse.text [0:len(handle)+1])
          if "@%s" % handle == previousResponse.text [0:len(handle)+1]: 
            tweetIDs.append (tweet.id)               	
	
          if tweet.id in tweetIDs or tweet.user.screen_name == 'awitnesstree':
            print ('Questions was already answered.')
          else: 
            response = random.sample (list (responses ['reply'] [5:len(responses)]), 1) [0]  
            tweet = api.update_status (status = "@%s "% (handle) + response, 
                                       in_reply_to_status_id = tweet.id)
            tweetIDs.append (tweet.id) # add it to the replied to IDs after first reply.
            responseCount = responseCount + 1
		      
print (str (now) + '; python: (2.5)  Responded to ' + str (responseCount) + 
       ' questions (i.e., how are you?).')

# look for selfie requests
#----------------------------------------------------------------------------------------
question = 'send me a selfie'
tmpTweets = api.search_tweets (q = "@%s " % (user.screen_name) + question)
tweets = []
responseCount = 0
for tweet in tmpTweets:
  local_dt = tweet.created_at
  questionTime = local_dt.astimezone (pytz.utc)
  if questionTime > lastResponseTime: tweets.append (tweet)	
 
# prepare response only once, including uploading the figure
#----------------------------------------------------------------------------------------
if tweets:
  response = responses ['reply'] [0]
  figureName = './images/witnesstree_PhenoCamImageRecent.jpg'
  file = api.media_upload (filename = figureName)
   
  # respond to selfie tweets
  #--------------------------------------------------------------------------------------
  for tweet in tweets:
    if tweet.id in tweetIDs or tweet.user.screen_name == 'awitnesstree':
      print ('Questions was already answered.')
    else: 
      handle = tweet.user.screen_name
      tweet = api.update_status (status = "@%s "% handle + response, 
                                 in_reply_to_status_id = tweet.id,
                                 media_ids = [file.media_id])
      tweetIDs.append (tweet.id) # Add it to the replied to IDs after first reply
      responseCount = responseCount + 1

# log entry
#----------------------------------------------------------------------------------------
print (str (now) + '; python: (2.6)  Responded to ' + str (responseCount) + 
       ' questions including selfies.')

# look for tweets containing "How old are you"
#----------------------------------------------------------------------------------------
question = 'How old are you'
tmpTweets = api.search_tweets (q = "@%s " % (user.screen_name) + question)
tweets = []
responseCount = 0
for tweet in tmpTweets:
  local_dt = localTwitter.localize (tweet.created_at, is_dst = None)
  questionTime = local_dt.astimezone (pytz.utc)
  if questionTime > lastResponseTime: tweets.append (tweet)	
    
# select response
#----------------------------------------------------------------------------------------
if tweets:
  response = responses ['reply'] [1]

  # respond to tweets containing "How old are you"
  #--------------------------------------------------------------------------------------
  for tweet in tweets:
    if tweet.id in tweetIDs or tweet.user.screen_name == 'awitnesstree':
      print ('Questions was already answered.')
    else: 
      handle = tweet.user.screen_name
      tweet = api.update_status (status = "@%s "% handle + response, 
                                 in_reply_to_status_id = tweet.id)
      tweetIDs.append (tweet.id) # Add it to the replied to IDs after first reply.
      responseCount = responseCount + 1

print (str (now) + '; python: (2.7)  Responded to ' + str (responseCount) + ' questions including age.')

# look for tweets containing "What was coldest day"
#------------------------------------------------------------------------------
question = 'What was coldest day'
tmpTweets = api.search_tweets (q = "@%s " % (user.screen_name) + question)
tweets = []
responseCount = 0
for tweet in tmpTweets:
  local_dt = localTwitter.localize (tweet.created_at, is_dst = None)
  questionTime = local_dt.astimezone (pytz.utc)
  if questionTime > lastResponseTime: tweets.append (tweet)	
  
# respond to tweets containing "What was coldest day"
#----------------------------------------------------------------------------------------
for tweet in tweets:
	if tweet.id in tweetIDs or tweet.user.screen_name == 'awitnesstree':
		print ('Questions was already answered.')
	else: 
		handle = tweet.user.screen_name
		response = responses ['reply'] [2]
		tweet = api.update_status (status = "@%s "% handle + response, 
                               in_reply_to_status_id = tweet.id)
		tweetIDs.append (tweet.id) # Add it to the replied to IDs after first reply.
		responseCount = responseCount + 1
		
print (str (now) + '; python: (2.8)  Responded to ' + str (responseCount) + ' questions including coldest day.')

# look for tweets containing "What was hottest day"
#------------------------------------------------------------------------------
question = 'What was hottest day'
tmpTweets = api.search_tweets (q = "@%s " % (user.screen_name) + question)
tweets = []
responseCount = 0
for tweet in tmpTweets:
  local_dt = localTwitter.localize (tweet.created_at, is_dst = None)
  questionTime = local_dt.astimezone (pytz.utc)
  if questionTime > lastResponseTime: tweets.append (tweet)	

# respond to tweets containing "What was hottest day"
#----------------------------------------------------------------------------------------
for tweet in tweets:
	if tweet.id in tweetIDs or tweet.user.screen_name == 'awitnesstree':
		print ('Questions was already answered.')
	else: 
		handle = tweet.user.screen_name
		response = responses ['reply'] [3]
		tweet = api.update_status (status = "@%s "% handle + response, 
                               in_reply_to_status_id = tweet.id)
		tweetIDs.append (tweet.id) # Add it to the replied to IDs after first reply.
		responseCount = responseCount + 1
		
print (str (now) + '; python: (2.9)  Responded to ' + str (responseCount) + ' questions including hottest day.')

# look for tweets containing "if a tree falls in the woods"
#------------------------------------------------------------------------------
question = 'if a tree falls in a forest, does it make a sound'
tmpTweets = api.search_tweets (q = "@%s " % (user.screen_name) + question)
tweets = []
responseCount = 0
for tweet in tmpTweets:
  local_dt = localTwitter.localize (tweet.created_at, is_dst = None)
  questionTime = local_dt.astimezone (pytz.utc)
  if questionTime > lastResponseTime: tweets.append (tweet)	
  
# respond to tweets containing "if a tree falls in the woods"
#----------------------------------------------------------------------------------------
for tweet in tweets:
	if tweet.id in tweetIDs or tweet.user.screen_name == 'awitnesstree':
		print ('Questions was already answered.')
	else: 
		handle = tweet.user.screen_name
		response = responses ['reply'] [4]
		tweet = api.update_status (status = "@%s "% handle + response, 
                               in_reply_to_status_id = tweet.id)
		tweetIDs.append (tweet.id) # Add it to the replied to IDs after first reply.
		responseCount = responseCount + 1
		
print (str (now) + '; python: (2.10) Responded to ' + str (responseCount) + ' questions including "if a tree falls...".')

# Update the memory.csv file to contain the timestamp, when we last replied to 
# a question to avoid trying to re-post
#----------------------------------------------------------------------------------------
local_dt = local.localize (datetime.now (), is_dst = None)
tmpMem ['lastResponse'] = datetime.strftime (local_dt, '%Y-%m-%d %H:%M %Z')
export_csv = tmpMem.to_csv (path + 'code/memory.csv', index = None, header = True, 
                            quoting = csv.QUOTE_NONE)
print (str (now) + '; python: (2.11) Updated lastResponse timestamp.')

# To delete a status use:'''
#----------------------------------------------------------------------------------------
#api.destroy_status('tweet ID')# Example: api.destroy_status(1007329170437951488) tweet ID can be found on URL after clicking on the Tweet

# These next few lines will print any tweets that were previously tweeted on the account '''
#------------------------------------------------------------------------------#
#public_tweets = api.home_timeline()
#for tweet in public_tweets:
#                      print(tweet.text)

# Should the witness tree follow its followers?
# To follow-back or unfollow all of the followers of the specific twitter page'''
#------------------------------------------------------------------------------#
#for follower in tweepy.Cursor(api.followers).items():
#    follower.follow()
#    print ("Followed everyone that is following " + user.name)


#for follower in tweepy.Cursor(api.followers).items():
#    follower.unfollow()
#    print ("unfollowed everyone that is following " + user.name)
