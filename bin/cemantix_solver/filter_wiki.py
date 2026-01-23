def load_known_words(path):
    with open(path, "r", encoding="utf-8") as f:
        return set(w.strip() for w in f if w.strip())


def filter_fasttext(vec_in, vec_out, known_words):
    kept = []

    with open(vec_in, "r", encoding="utf-8", errors="ignore") as fin:
        header = fin.readline().strip()
        _, dim = map(int, header.split())

        for line in fin:
            parts = line.rstrip().split(" ", 1)
            if len(parts) != 2:
                continue

            word, values = parts
            if word in known_words:
                kept.append((word, values))

    # écriture avec nouvel en-tête
    with open(vec_out, "w", encoding="utf-8") as fout:
        fout.write(f"{len(kept)} {dim}\n")
        for word, values in kept:
            fout.write(f"{word} {values}\n")

    print(f"✔ {len(kept)} mots conservés")


if __name__ == "__main__":
    mots = load_known_words("fr_fat_list.txt")
    filter_fasttext(
        vec_in="wiki.fr.vec",
        vec_out="wiki.fr.filtered.vec",
        known_words=mots
    )
