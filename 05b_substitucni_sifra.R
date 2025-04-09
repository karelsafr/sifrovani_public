#####################################################################
############ Karel Safr, statisticka cryptanalyza, kodovani: UTF8
############ 05_2 Bayesovské/statistické metody a optimalizační přístupy

# M-H algoritmus Bayes
# Gibbs sampling (varianta M-H)
# Deterministickou greedy optimalizaci (koordinátní ascent)
# E-M algoritmus
# Genetický algoritmus
# PSO algoritmus
# Nested sampling
# Simulované žíhání


#####################################################################
#####################################################################
# Explicitní Bayesovský přístup MH algoritmus
# Bayesovské prolomení substituční šifry pomocí MH algoritmu
# dorbné úpravy k efektivitě
prolom_substitute_bayes <- function(text, TM, iter, start = sample(0:26)){
  
  # současný stav
  guess_current = start
  text_current = substitute_decrypt(text, guess_current)
  log_lik_current = plausibility(text_current, TM)
  
  for(i in 1:iter){
    
    # symetrické návrhové rozdělení: prohodíme dvě náhodná písmena v klíči
    ij = sample(1:27, 2)
    guess_new = guess_current
    guess_new[ij] = guess_current[ij[2:1]]
    
    # nový stav
    text_new = substitute_decrypt(text, guess_new)
    log_lik_new = plausibility(text_new, TM)
    
    # logaritmický akceptační poměr kvůli numerické stabilitě
    log_accept_ratio = log_lik_new - log_lik_current
    
    # rozhodnutí o akceptaci (Bayesovský krok MH algoritmu)
    if(log(runif(1)) < log_accept_ratio){
      guess_current = guess_new
      log_lik_current = log_lik_new
      text_current = text_new
    }
    
    # Průběžné výpisy každých 100 iterací pro kontrolu konvergence
    if(i %% 100 == 0){
      cat(sprintf("Iterace %d: Log-likelihood textu: %.2f\n", i, log_lik_current))
    }
    
  }
  
  # Výsledný posteriorní odhad klíče a textu
  list(klíč = guess_current, 
       text = text_current, 
       log_lik = log_lik_current)
}

# Příklad použití:
set.seed(123)
key_real = sample(0:26)
text_encrypted = substitute_encrypt(jlondon, key_real)

# prolomení šifry:
result = prolom_substitute_bayes(text_encrypted, TM, iter = 10000)

# Výsledky:
cat("Posteriorní klíč:\n", result$klíč, "\n")
cat("Dešifrovaný text:\n", substr(result$text,1,500), "\n") # prvních 500 znaků textu

# Ověření správnosti:
matches = sum(result$klíč == key_real)
cat(sprintf("Počet správně určených substitucí v klíči: %d z %d\n", matches, length(key_real)))

success_rate <- sum(strsplit(result$text, "")[[1]] == strsplit(jlondon, "")[[1]]) / nchar(jlondon)
cat(sprintf("\nÚspěšnost dešifrování: %.2f%%\n", success_rate * 100))

#####################################################################
#####################################################################
# MH algoritmus: Gibbs sampler funkce pro prolomení šifer

gibbs_substitution_break <- function(enc_text, TM, iter, start_key = sample(0:26)){
  current_key <- start_key
  
  for (it in 1:iter){
    for(i in 1:27){
      plausibilities <- numeric(27)
      keys_test <- matrix(rep(current_key, 27), nrow=27, byrow=TRUE)
      
      # Výměna substitucí: swap i ↔ j
      for(j in 1:27){
        test_key <- current_key
        # prohoď substituci písmen i a j
        tmp <- test_key[i]
        test_key[i] <- test_key[j]
        test_key[j] <- tmp
        
        decrypted_try <- substitute_decrypt(enc_text, test_key)
        plausibilities[j] <- plausibility(decrypted_try, TM)
      }
      
      # stabilizace pravděpodobností
      max_p <- max(plausibilities)
      probs <- exp(plausibilities - max_p)
      probs <- probs / sum(probs)
      
      # nový swap
      new_j <- sample(1:27, 1, prob = probs)
      
      # aktualizuj klíč swapem i ↔ new_j
      tmp <- current_key[i]
      current_key[i] <- current_key[new_j]
      current_key[new_j] <- tmp
    }
    
    # průběžné info každých 10 iterací
    if(it %% 10 == 0){
      decrypted_text <- substitute_decrypt(enc_text, current_key)
      current_plausibility <- plausibility(decrypted_text, TM)
      cat(sprintf("Iterace %d, plausibilita: %.2f\n", it, current_plausibility))
    }
  }
  
  decrypted_text <- substitute_decrypt(enc_text, current_key)
  current_plausibility <- plausibility(decrypted_text, TM)
  list(final_key = current_key, decrypted_text = decrypted_text, final_plausibility = current_plausibility)
}


# ukázak použití
set.seed(3)


res_gibbs <- gibbs_substitution_break(enc, TM, iter = 300)

# Výsledky
cat("\nOdhadnutý klíč:\n")
print(res_gibbs$final_key)

cat("\nDešifrovaný text:\n")
cat(res_gibbs$decrypted_text)


success_rate <- sum(strsplit(res_gibbs$decrypted_text, "")[[1]] == strsplit(jlondon, "")[[1]]) / nchar(jlondon)
cat(sprintf("\nÚspěšnost dešifrování: %.2f%%\n", success_rate * 100))



#####################################################################
#####################################################################
# Deterministická greedy optimalizace (koordinátní ascent):

# varianta na variational substituci - ale ne plnohodnotná

variational_substitution_break <- function(enc_text, TM, iter = 200){
  current_key <- sample(0:26)
  
  for (it in 1:iter){
    updated <- FALSE
    for(i in 1:27){
      log_probs <- numeric(27)
      
      for(j in 1:27){
        test_key <- current_key
        # prohození substitucí (swap)
        test_key[c(i,j)] <- test_key[c(j,i)]
        decrypted_try <- substitute_decrypt(enc_text, test_key)
        log_probs[j] <- plausibility(decrypted_try, TM)
      }
      
      max_log <- max(log_probs)
      probs <- exp(log_probs - max_log)
      probs <- probs / sum(probs)
      
      # Deterministická (variační) aktualizace na nejpravděpodobnější swap:
      best_j <- which.max(probs)
      
      if(best_j != i){
        current_key[c(i,best_j)] <- current_key[c(best_j,i)]
        updated <- TRUE
      }
    }
    
    decrypted_text <- substitute_decrypt(enc_text, current_key)
    current_plausibility <- plausibility(decrypted_text, TM)
    
    cat(sprintf("Iterace %d, plausibilita: %.2f\n", it, current_plausibility))
    
    if(!updated){
      cat("Konvergence dosažena (žádná změna).\n")
      break
    }
  }
  
  list(
    final_key = current_key,
    decrypted_text = decrypted_text,
    final_plausibility = current_plausibility
  )
}



# Použití
set.seed(3)
res_var <- variational_substitution_break(enc, TM, iter = 100)

cat("\nOdhadnutý klíč (variační inference):\n")
print(res_var$final_key)

cat("\nDešifrovaný text:\n")
cat(res_var$decrypted_text)

success_rate <- sum(strsplit(res_var$decrypted_text, "")[[1]] == strsplit(jlondon, "")[[1]]) / nchar(jlondon)
cat(sprintf("\nÚspěšnost dešifrování: %.2f%%\n", success_rate * 100))


##########################################################
##########################################################
# EM algoritmus:

em_substitution_break <- function(enc_text, TM, iter = 100){
  
  # Inicializace: náhodný klíč
  current_key <- sample(0:26)
  prev_plausibility <- -Inf
  
  for(it in 1:iter){
    
    # ----- E-krok (Expectation): -----
    # Dekódujeme aktuálním klíčem (aproximace posterioru)
    decrypted_text <- substitute_decrypt(enc_text, current_key)
    
    # V našem případě je posterior aproximován deterministicky dekódovaným textem.
    
    # ----- M-krok (Maximization): -----
    # Najdeme nový klíč greedy optimalizací založenou na dekódovaném textu z E-kroku
    new_key <- current_key
    improved <- FALSE
    
    for(i in 1:27){
      log_probs <- numeric(27)
      
      for(j in 1:27){
        test_key <- new_key
        # Prohození substitucí (swap)
        test_key[c(i,j)] <- test_key[c(j,i)]
        decrypted_try <- substitute_decrypt(enc_text, test_key)
        log_probs[j] <- plausibility(decrypted_try, TM)
      }
      
      # Najdi swap, který maximalizuje plausibilitu
      best_j <- which.max(log_probs)
      
      if(best_j != i){
        new_key[c(i,best_j)] <- new_key[c(best_j,i)]
        improved <- TRUE
      }
    }
    
    # Vyhodnocení nového klíče
    decrypted_text <- substitute_decrypt(enc_text, new_key)
    current_plausibility <- plausibility(decrypted_text, TM)
    
    cat(sprintf("EM iterace %d, plausibilita: %.2f\n", it, current_plausibility))
    
    # Pokud plausibilita neklesá, aktualizuj klíč
    if(current_plausibility > prev_plausibility){
      current_key <- new_key
      prev_plausibility <- current_plausibility
    } else {
      cat("Konvergence dosažena (žádné zlepšení plausibility).\n")
      break
    }
    
    if(!improved){
      cat("Konvergence dosažena (žádný lepší swap nalezen).\n")
      break
    }
  }
  
  list(
    final_key = current_key,
    decrypted_text = decrypted_text,
    final_plausibility = current_plausibility
  )
}


set.seed(3)
res_var <- em_substitution_break(enc, TM, iter = 100)

cat("\nOdhadnutý klíč (variační inference):\n")
print(res_var$final_key)

cat("\nDešifrovaný text:\n")
cat(res_var$decrypted_text)

success_rate <- sum(strsplit(res_var$decrypted_text, "")[[1]] == strsplit(jlondon, "")[[1]]) / nchar(jlondon)
cat(sprintf("\nÚspěšnost dešifrování: %.2f%%\n", success_rate * 100))





##########################################################
##########################################################
### Genetický algoritmus:
# Genetický algoritmus na prolomení substituční šifry


genetic_substitution_break <- function(enc_text, TM, pop_size = 50, iter = 100, mutation_rate = 0.1){
  
  # Inicializace populace (náhodné klíče)
  population <- replicate(pop_size, sample(0:26), simplify = FALSE)
  
  # Výpočet fitness
  fitness <- function(key){
    text <- substitute_decrypt(enc_text, key)
    plausibility(text, TM)
  }
  
  # Iterace genetického algoritmu
  for(gen in 1:iter){
    
    # Hodnocení všech jedinců
    fitness_values <- sapply(population, fitness)
    
    # Výběr nejlepších (selekce)
    ranked_idx <- order(fitness_values, decreasing = TRUE)
    population <- population[ranked_idx]
    fitness_values <- fitness_values[ranked_idx]
    
    # Výpis nejlepšího fitness každých 10 generací
    if(gen %% 10 == 0){
      cat(sprintf("Generace %d, nejlepší plausibilita: %.2f\n", gen, fitness_values[1]))
    }
    
    # Nová generace
    new_population <- population[1:(pop_size/2)] # Elitismus - ponecháme polovinu nejlepších
    
    # Křížení (crossover)
    while(length(new_population) < pop_size){
      parents <- sample(new_population, 2, replace = FALSE)
      
      # Jednobodový crossover
      cross_point <- sample(2:26, 1)
      child1 <- c(parents[[1]][1:cross_point], setdiff(parents[[2]], parents[[1]][1:cross_point]))
      child2 <- c(parents[[2]][1:cross_point], setdiff(parents[[1]], parents[[2]][1:cross_point]))
      
      # Přidej potomky
      new_population <- c(new_population, list(child1, child2))
    }
    
    # Mutace
    for(i in 2:pop_size){ # mutaci nepoužijeme u nejlepšího jedince (elitismus)
      if(runif(1) < mutation_rate){
        swap_idx <- sample(1:27, 2)
        tmp <- new_population[[i]][swap_idx[1]]
        new_population[[i]][swap_idx[1]] <- new_population[[i]][swap_idx[2]]
        new_population[[i]][swap_idx[2]] <- tmp
      }
    }
    
    # Nastavíme novou generaci
    population <- new_population
  }
  
  # Vracíme nejlepší řešení
  best_key <- population[[1]]
  best_text <- substitute_decrypt(enc_text, best_key)
  best_plausibility <- fitness_values[1]
  
  list(
    final_key = best_key,
    decrypted_text = best_text,
    final_plausibility = best_plausibility
  )
}



set.seed(3)
res_var <- genetic_substitution_break(enc, TM, pop_size = 100, iter = 500,  mutation_rate = 0.1)

cat("\nOdhadnutý klíč (variační inference):\n")
print(res_var$final_key)

cat("\nDešifrovaný text:\n")
cat(res_var$decrypted_text)

success_rate <- sum(strsplit(res_var$decrypted_text, "")[[1]] == strsplit(jlondon, "")[[1]]) / nchar(jlondon)
cat(sprintf("\nÚspěšnost dešifrování: %.2f%%\n", success_rate * 100))





###########################################################
###########################################################
####  PSO algoritmus - Particle Swarm Optimization

pso_substitution_break <- function(enc_text, TM, num_particles = 30, iter = 100, alpha = 0.8){
  
  # Pomocná fitness funkce
  fitness <- function(key){
    text <- substitute_decrypt(enc_text, key)
    plausibility(text, TM)
  }
  
  # Inicializace hejna částic (náhodné klíče)
  particles <- replicate(num_particles, sample(0:26), simplify = FALSE)
  velocities <- replicate(num_particles, list(), simplify = FALSE)
  
  # Personal best (nejlepší nalezené řešení každé částice)
  pBest <- particles
  pBest_vals <- sapply(particles, fitness)
  
  # Global best (nejlepší řešení v celém hejnu)
  gBest_idx <- which.max(pBest_vals)
  gBest <- pBest[[gBest_idx]]
  gBest_val <- pBest_vals[gBest_idx]
  
  for(it in 1:iter){
    
    for(i in 1:num_particles){
      
      # Vytvoříme novou rychlost (swap operace):
      # Pohyb směrem k pBest
      if(!identical(particles[[i]], pBest[[i]])){
        diff_pb <- which(particles[[i]] != pBest[[i]])
        if(length(diff_pb) >= 2){
          velocities[[i]] <- list(sample(diff_pb, 2))
        }
      }
      
      # Pohyb směrem k gBest
      if(!identical(particles[[i]], gBest)){
        diff_gb <- which(particles[[i]] != gBest)
        if(length(diff_gb) >= 2 && runif(1) < alpha){
          velocities[[i]] <- c(velocities[[i]], list(sample(diff_gb, 2)))
        }
      }
      
      # Aplikuj rychlosti (swapy)
      for(swap in velocities[[i]]){
        tmp <- particles[[i]][swap[1]]
        particles[[i]][swap[1]] <- particles[[i]][swap[2]]
        particles[[i]][swap[2]] <- tmp
      }
      
      # Vyčisti rychlosti
      velocities[[i]] <- list()
      
      # Aktualizace pBest
      current_fitness <- fitness(particles[[i]])
      if(current_fitness > pBest_vals[i]){
        pBest[[i]] <- particles[[i]]
        pBest_vals[i] <- current_fitness
        
        # Aktualizace gBest
        if(current_fitness > gBest_val){
          gBest <- particles[[i]]
          gBest_val <- current_fitness
        }
      }
      
    } # konec iterace částic
    
    # Výstup každých 10 iterací
    if(it %% 10 == 0){
      cat(sprintf("Iterace %d, nejlepší plausibilita: %.2f\n", it, gBest_val))
    }
  }
  
  decrypted_text <- substitute_decrypt(enc_text, gBest)
  
  list(
    final_key = gBest,
    decrypted_text = decrypted_text,
    final_plausibility = gBest_val
  )
}




set.seed(3)
res_var <- pso_substitution_break(enc, TM, num_particles = 100, iter = 1000, alpha = 0.8)

cat("\nOdhadnutý klíč (variační inference):\n")
print(res_var$final_key)

cat("\nDešifrovaný text:\n")
cat(res_var$decrypted_text)

success_rate <- sum(strsplit(res_var$decrypted_text, "")[[1]] == strsplit(jlondon, "")[[1]]) / nchar(jlondon)
cat(sprintf("\nÚspěšnost dešifrování: %.2f%%\n", success_rate * 100))




###########################################################
###########################################################
#### Nested sampling (hnízděné vzorkování)

nested_sampling_break <- function(enc_text, TM, num_particles = 30, iter = 1000){
  
  # Inicializace částic (náhodné klíče)
  particles <- replicate(num_particles, sample(0:26), simplify = FALSE)
  particle_plausibilities <- sapply(particles, function(k) plausibility(substitute_decrypt(enc_text, k), TM))
  
  # Uložení nejlepšího řešení
  best_idx <- which.max(particle_plausibilities)
  best_particle <- particles[[best_idx]]
  best_plaus <- particle_plausibilities[best_idx]
  
  for(it in 1:iter){
    
    # Najdi nejhorší částici (nejnižší plausibilitu)
    worst_idx <- which.min(particle_plausibilities)
    worst_plaus <- particle_plausibilities[worst_idx]
    
    # Vyber náhodně jinou částici (jako předlohu pro novou částici)
    idx_other <- sample(setdiff(1:num_particles, worst_idx), 1)
    new_particle <- particles[[idx_other]]
    
    # Proveď náhodné prohození dvou substitucí, aby vznikla nová částice
    swap_idx <- sample(1:27, 2)
    tmp <- new_particle[swap_idx[1]]
    new_particle[swap_idx[1]] <- new_particle[swap_idx[2]]
    new_particle[swap_idx[2]] <- tmp
    
    # Ověř, zda nová částice má plausibilitu vyšší než odstraněná
    new_plaus <- plausibility(substitute_decrypt(enc_text, new_particle), TM)
    
    if(new_plaus > worst_plaus){
      # Nahradíme nejhorší částici novou částicí
      particles[[worst_idx]] <- new_particle
      particle_plausibilities[worst_idx] <- new_plaus
      
      # Aktualizace nejlepší částice
      if(new_plaus > best_plaus){
        best_particle <- new_particle
        best_plaus <- new_plaus
      }
    }
    
    # Průběžný výpis každých 100 iterací
    if(it %% 100 == 0){
      cat(sprintf("Iterace %d, aktuální nejlepší plausibilita: %.2f\n", it, best_plaus))
    }
  }
  
  # Výsledné řešení
  decrypted_text <- substitute_decrypt(enc_text, best_particle)
  
  list(
    final_key = best_particle,
    decrypted_text = decrypted_text,
    final_plausibility = best_plaus
  )
}



set.seed(3)
res_var <- nested_sampling_break(enc, TM, num_particles = 10, iter = 10000)

cat("\nOdhadnutý klíč (variační inference):\n")
print(res_var$final_key)

cat("\nDešifrovaný text:\n")
cat(res_var$decrypted_text)

success_rate <- sum(strsplit(res_var$decrypted_text, "")[[1]] == strsplit(jlondon, "")[[1]]) / nchar(jlondon)
cat(sprintf("\nÚspěšnost dešifrování: %.2f%%\n", success_rate * 100))



###########################################################
###########################################################
#### simulované žíhání


simulated_annealing_break <- function(enc_text, TM, iter = 5000, T_init = 10, T_final = 0.1){
  
  # Inicializace
  current_key <- sample(0:26)
  current_text <- substitute_decrypt(enc_text, current_key)
  current_plaus <- plausibility(current_text, TM)
  
  best_key <- current_key
  best_plaus <- current_plaus
  
  # Chladící schéma (exponenciální snižování teploty)
  T <- T_init
  cooling_rate <- (T_final / T_init)^(1 / iter)
  
  for(it in 1:iter){
    
    # Vygeneruj nové řešení (swap dvou náhodných substitucí)
    new_key <- current_key
    swap_idx <- sample(1:27, 2)
    new_key[swap_idx] <- new_key[rev(swap_idx)]
    
    new_text <- substitute_decrypt(enc_text, new_key)
    new_plaus <- plausibility(new_text, TM)
    
    # Rozhodnutí o přijetí (Metropolisovský krok)
    delta <- new_plaus - current_plaus
    
    if(delta > 0 || runif(1) < exp(delta / T)){
      current_key <- new_key
      current_plaus <- new_plaus
      
      # Aktualizace nejlepšího řešení
      if(current_plaus > best_plaus){
        best_key <- current_key
        best_plaus <- current_plaus
      }
    }
    
    # Snižování teploty
    T <- T * cooling_rate
    
    # Průběžný výpis
    if(it %% 500 == 0){
      cat(sprintf("Iterace %d, nejlepší plausibilita: %.2f, aktuální teplota: %.4f\n", it, best_plaus, T))
    }
  }
  
  decrypted_text <- substitute_decrypt(enc_text, best_key)
  
  list(
    final_key = best_key,
    decrypted_text = decrypted_text,
    final_plausibility = best_plaus
  )
}


set.seed(3)
res_var <- simulated_annealing_break(enc, TM, iter = 5000, T_init = 10, T_final = 0.1)

cat("\nOdhadnutý klíč (variační inference):\n")
print(res_var$final_key)

cat("\nDešifrovaný text:\n")
cat(res_var$decrypted_text)

success_rate <- sum(strsplit(res_var$decrypted_text, "")[[1]] == strsplit(jlondon, "")[[1]]) / nchar(jlondon)
cat(sprintf("\nÚspěšnost dešifrování: %.2f%%\n", success_rate * 100))
