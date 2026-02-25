<p align="center">
  <img src="Images/lgbtq_movies_hex_final.png" width="180">
</p>

# 🌈 Queer Stories Through Data: Highlighting LGBTQ+ Cinema with R and Shiny

## 🎤 [rainbowR Conference Talk](https://conference.rainbowr.org/)

🌈 **LGBTQ+ Movies Explorer** is an interactive Shiny application built in R to explore patterns of LGBTQ+ representation in global cinema.

Using the openly available [**tidyrainbow LGBTQ+ Movie Database**](https://github.com/r-lgbtq/tidyrainbow/tree/main/data/LGBTQ-movie-database) (derived from [TMDb](https://www.themoviedb.org/about) metadata),  

Data curated by: [Cara Cuiule](https://github.com/cacalc/tidyRainbowScratch)

The app enables users to:

- 📈 Explore trends in queer storytelling over time  
- 🎭 Analyse genre and language patterns  
- 📝 Identify recurring themes in film descriptions  
- 💬 Interact with the dataset through a conversational chatbot interface

---

## 🚀 How to Run Locally

This application follows the **{golem}** framework for Shiny apps and uses **{renv}** to ensure reproducible package versions.

### 1️⃣ Clone the repository

```bash
git clone https://github.com/LucyNjoki/LGBTQ-Movies.git
cd LGBTQ-Movies
```

2️⃣ Install dependencies

Open the project in RStudio and run:

```r
install.packages("renv")   # if not already installed
renv::restore()            # installs exact package versions from renv.lock
```

3️⃣ Run the application

```r
devtools::load_all(".") # to run the app
```

---

## 🌐 Collaboration

This project was developed as a collaboration between  
**[Njoki Njuki](https://www.linkedin.com/in/lucy-njoki/)** and **[Tamires Martins](https://www.linkedin.com/in/tamimart/)**, bringing together different lived perspectives and shared commitments to queer visibility and inclusive data practices.

We warmly welcome contributions, feedback, and collaboration from the community.

If you’re interested in:
- Expanding the dataset  
- Improving app features  
- Enhancing NLP tagging or analytics  
- Exploring new research directions  

Please feel free to open an issue or get in touch ✨


