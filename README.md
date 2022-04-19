# webdubois: W.E.B. Du Bois Challenge plots <img align="right" src="https://github.com/IcaroBernardes/webdubois/blob/main/portrait.jpg" alt="logo" width="200">

[W.E.B. Du Bois](https://en.wikipedia.org/wiki/W._E._B._Du_Bois) (February 23, 1868 – August 27, 1963) was an American sociologist, socialist, historian
and Pan-Africanist civil rights activist (*Source: Wikpedia*). 

In the 1900 Paris Exposition his team made a historical display. They presented in many charts the situation of Black people in Georgia and the US in general.
The timelessness of these posters provoked Anthony Starks, Allen Hillery and Sekou Tyler to create the
[#DuBoisChallenge2022](https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2022). The proposal was to reproduce Du Bois posters with any tools
whatsoever. I chose to make these charts in `R` using `ggplot` and some of its extensions. Furthermore, like Du Bois I used this opportunity to talk about racial
inequality in my own country, Brazil.

However this effort does not end here. I started to make templates out of theses plots, so others can make Du Bois styled plots using their own data. The `R` package `dubois` already has one function: https://github.com/IcaroBernardes/dubois. Soon I intend to provide details about my struggles to make all this.

## 2022
### Week 10 - Racial inequality (Black candidates and elected)
💻 Code: https://github.com/IcaroBernardes/webdubois/blob/main/2022/week10/week10.R

🌍 Twitter thread: https://twitter.com/IcaroBSC/status/1514299766271721475

![](https://github.com/IcaroBernardes/webdubois/blob/main/2022/week10/elections.png)

### Week 09 - Racial inequality (Occupied blacks in Brazil by educational level)
💻 Code: https://github.com/IcaroBernardes/webdubois/blob/main/2022/week09/week09.R

🌍 Twitter thread: https://twitter.com/IcaroBSC/status/1511072124357775367

![](https://github.com/IcaroBernardes/webdubois/blob/main/2022/week09/job.png)

### Week 08 - Racial inequality (Rate of homicides)
💻 Code: https://github.com/IcaroBernardes/webdubois/blob/main/2022/week08/week08.R

🌍 Twitter thread: https://twitter.com/IcaroBSC/status/1508975839618215940

![](https://github.com/IcaroBernardes/webdubois/blob/main/2022/week08/violence.png)

### Week 07 - Racial inequality (Political candidatures financing)
💻 Code: https://github.com/IcaroBernardes/webdubois/blob/main/2022/week07/week07.R

🌍 Twitter thread: https://twitter.com/IcaroBSC/status/1506334651538972681

![](https://github.com/IcaroBernardes/webdubois/blob/main/2022/week07/congress.png)

### Week 06 - Racial inequality (Illiteracy)
💻 Code: https://github.com/IcaroBernardes/webdubois/blob/main/2022/week06/week06.R

🌍 Twitter thread: https://twitter.com/IcaroBSC/status/1503890070029451270

![](https://github.com/IcaroBernardes/webdubois/blob/main/2022/week06/illiteracy.png)

### Week 05 - Racial inequality (Managerial job positions)
💻 Code: https://github.com/IcaroBernardes/webdubois/blob/main/2022/week05/week05.R

🌍 Twitter thread: https://twitter.com/IcaroBSC/status/1501323176088780800

![](https://github.com/IcaroBernardes/webdubois/blob/main/2022/week05/managers.png)

### Week 04 - Racial inequality (Real income of people in informal jobs)
💻 Code: https://github.com/IcaroBernardes/webdubois/blob/main/2022/week04/week04.R

🌍 Twitter thread: https://twitter.com/IcaroBSC/status/1498459321243475968

![](https://github.com/IcaroBernardes/webdubois/blob/main/2022/week04/incomes.png)

### Week 03 - Racial inequality (Mobile phones and internet access)
💻 Code: https://github.com/IcaroBernardes/webdubois/blob/main/2022/week03/week03.R

🌍 Twitter thread: https://twitter.com/IcaroBSC/status/1496229656902926336

![](https://github.com/IcaroBernardes/webdubois/blob/main/2022/week03/access.png)

### Week 02 - Racial inequality (Real income of households)
💻 Code: https://github.com/IcaroBernardes/webdubois/blob/main/2022/week02/week02.R

🌍 Twitter thread: https://twitter.com/IcaroBSC/status/1493768750755041285

![](https://github.com/IcaroBernardes/webdubois/blob/main/2022/week02/income.png)

### Week 01 - Racial inequality (The Black Diaspora)
💻 Code: https://github.com/IcaroBernardes/webdubois/blob/main/2022/week01/week01.R

🌍 Twitter thread: https://twitter.com/IcaroBSC/status/1516517164727119877

![](https://github.com/IcaroBernardes/webdubois/blob/main/2022/week01/enslaved.png)

### Week 00 - Tuskegee Airmen
💻 **Code:** https://github.com/IcaroBernardes/webdubois/blob/main/2022/week00/week00.R

🌍 **Twitter thread:** https://twitter.com/IcaroBSC/status/1491559819656024071

<img align="right" src="https://github.com/IcaroBernardes/webdubois/blob/main/originals/original-plate-21.jpg" alt="logo" width="200">
<p align="justify"> 💬 <b>About this plot:</b> The first plot I did was for both the TidyTuesday and the Du Bois Challenge. They provided us with data about pilots from the Tuskgee Airmen. Seeing the data and the originals from Du Bois, I decided to make a line plot of the number of enemy planes downed by the pilots. The poster I took inspiration from had two periods represented by one line completely in black and another with black borders and the same color as the paper. <code>ggplot</code> lines geometries (like line, step, etc.) do not have a "border" option. Since I didn't know about <a href="https://wurli.github.io/ggborderline/"><code>ggborderline</code></a>, I had to give the illusion of a border to my lines. I created this illusion by placing two lines in the same coordinates, one thicker in black behind another that was lighter and had the background color. Then, I browsed Wikipedia to find context about the numbers of the Tuskgee. These labels were glued to the poster in the original, so the grid is hidden behind them. <a href="https://wilkelab.org/ggtext/"><code>ggtext</code></a> helped me to replicate this effect by creating a box behind the texts in the background color. To make the grid I created a sequence of lines in red and colored the first and last in black. Same thing for the legend in the left. The most interesting detail in it is the repeated icon for the "downed planes". For that I used <a href="https://cran.r-project.org/web/packages/emojifont/vignettes/emojifont.html"><code>emojifont</code></a>.
</p>

![](https://github.com/IcaroBernardes/webdubois/blob/main/2022/week00/strikes.png)
