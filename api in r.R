
require(C50)
library(httr)
library(jsonlite)
#* Echo back the output
#* @get /echo
function(msg=""){
  #Ali Api
  API_URL <- paste0('https://jsonplaceholder.typicode.com/todos/1')
  raw_data <- GET(API_URL)
  data<-fromJSON(rawToChar(raw_data$content))
  names(data)
  
  #Train data
  anxiety_data<-read.csv('https://zenodo.org/record/6912705/files/Sample_depression.csv')
  View(anxiety_data)
  want_data<-anxiety_data[,c(-1,-2,-8,-15,-36,-37)]
  want_data$dep2<-factor(want_data$dep2)
  View(want_data)
  train <- want_data[1:352,]
  
  #Test data
  fake<-data.frame(sex=c("1"),
                   Age=c("28"),
                   Married=c("1"),
                   Number_children=c("3"),
                   total_members=c("5"),
                   On.a.scale.of.1.100..how.would.you.express.this.feeling.=c("50"),
                   Are.you.happy.with.your.financial.state.=c("No"),
                   Understanding.with.your.family.members.=c("Normal"),
                   Are.you.feeling.pressure.in.your.study.or.work.right.now.=c("Yes"),
                   Are.you.happy.with.your.living.place.=c("No"),
                   Who.supports.you.when.you.are.not.succeeding.in.your..life.=c("Family"),
                   Do.you.have.inferiority.complex.=c("Maybe"),
                   Are.you.satisfied.with.your.meal.today.=c("Neutral"),
                   Are.you.feeling.sick.health.issues.today.=c("No"),
                   Have.you.done.any.recreational.activity..sports..gaming..hobby.etc...today.=c("Yes"),
                   How.long.did.you.sleep.last.night..in.hours.=c("7.0"),
                   Educational.Level=c("Master"),
                   Little.interest.or.pleasure.in.doing.things=c("2"),
                   Feeling.down..depressed..or.hopeless=c("3"),
                   Trouble.falling.or.staying.asleep..or.sleeping.too.much=c("4"),
                   Feeling.tired.or.having.little.energy=c("2"),
                   Poor.appetite.or.overeating=c("1"),
                   Feeling.bad.about.yourself.or.that.you.are.a.failure.or.not.have.let.yourself.or.your.family.down=c("2"),
                   Trouble.concentrating.on.things..such.as.reading.the.newspaper.or.watching.television=c("4"),
                   Moving.or.speaking.so.slowly.that.other.people.could.have.noticed.Or.being.so.restless.that.you.have.been.moving.around.a.lot.more.than.usual=c("2"),
                   Thoughts.that.you.would.be.better.off.dead.or.of.hurting.yourself.in.some.way=c("2"),
                   Do.you.have.part.time.or.full.time.job.=c("Part time"),
                   Which.of.the.following.best.describes.your.term.time.accommodation.=c("Home (with parents)"),
                   How.many.hours.do.you.spend.studying.each.day.=c("2 - 4 hours"),
                   How.many.of.the.electronic.gadgets..e.g..mobile.phone..computer..laptop..PSP..PS4..Wii..etc...do.you.have.in.your.home.or.your.student.accommodation.mess.hall.=c("More than 6"),
                   How.many.hours.do.you.spend.on.social.media.per.day.=c("2 - 4 Hours"))
  
  View(fake)
  test <- fake[1,]
  
  
  
  #Classification Model
  m1<- C5.0(train[,-32],train[,32])
  m1
  summary(m1)
  
  
  p1 <- predict(m1, test)
  list(msg=paste0(p1))
}


