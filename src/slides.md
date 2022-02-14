---
marp: true
theme: gaia
_class: lead,
paginate: false
backgroundColor: #fff
backgroundImage: url('https://marp.app/assets/hero-background.svg')
---

# Wciąż nienazwany język funkcyjny kompilowany do JavaScripta

--- 
# Co można w nim napisać?
---
# Jak uruchamiany jest kod?
![height: 200px](/home/brych/uni/pf/lang/pipeline.png)

---
# Odsładzanie
Dlaczego robić sobie dodatkowy krok?

---
# Odsładzanie
Dlaczego robić sobie dodatkowy krok?
Uproszczenie kodu upraszcza analizę

---

```
data List a  
= Empty
| Cons a (List a)

let append xs ys = match xs with 
| Empty    -> ys
| Cons h t -> Cons h (append t ys)
end
```

---

```
let append = 
    fix (fun append -> 
        (fun xs -> 
        (fun ys -> 
            switch xs on 
                "Empty" -> ys
                "Cons"  -> 
                    let h = getCons0 xs in 
                    let t = getCons1 xs in 
                    (Cons h (append y ys)))))
```



---
# Wyzwania kompilacji do JS

---

# Rekursja ogonowa

- Oddajmy kontrolę nad wywoływaniem funkcji trampolinie
- (może zobaczmy kod źródłowy)

---

# Let-wyrażenia, lub jak przestałem się martwić i uwierzyłem w V8

---

```let x = 10 in print x```
Jak przetłumaczyć to wyrażenie na JS?


---

```let x = 10 in print x```
Jak przetłumaczyć to wyrażenie na JS?
``` 
{
    let x = 10;
    print(x);
}
```

---

```let x = 10 in print x```
Jak przetłumaczyć to wyrażenie na JS?
``` 
{
    let x = 10;
    print(x);
}
```
A co jeśli to by było wyrażenie?

---

```2 + (let x = 10 in x)```
Jak przetłumaczyć to wyrażenie na JS?
``` 
{
    let x = 10;
    // ???
}
```

---

```2 + (let x = 10 in x)```
Jak przetłumaczyć to wyrażenie na JS?
``` 
{
    let x = 10;
    // ???
}
```
Na szczęście w JavaScripcie mamy jeszcze jeden mechanizm wprowadzający zakres zmiennych: funkcje

---
```2 + (let x = 10 in x)```
Jak przetłumaczyć to wyrażenie na JS?
``` 
(function(){
    let x = 10;
    return 2 + x;
})()
```
Zaufajmy, że V8 nie jest głupie, i da radę to wyoptymalizować

---

# Pytania