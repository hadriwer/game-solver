import unicodedata

def supprimer_accents(fichier_entree, fichier_sortie):
    try:
        with open(fichier_entree, 'r', encoding='utf-8') as f:
            contenu = f.read()

        # Normalisation NFD : décompose les caractères (ex: 'é' devient 'e' + '´')
        # On ne garde que les caractères qui ne sont pas des marques d'accentuation
        contenu_sans_accent = ''.join(
            char for char in unicodedata.normalize('NFD', contenu)
            if unicodedata.category(char) != 'Mn'
        )

        with open(fichier_sortie, 'w', encoding='utf-8') as f:
            f.write(contenu_sans_accent)

        print(f"Succès ! Le fichier a été sauvegardé sous : {fichier_sortie}")

    except FileNotFoundError:
        print("Erreur : Le fichier d'entrée n'a pas été trouvé.")
    except Exception as e:
        print(f"Une erreur est survenue : {e}")

# Utilisation du script
nom_fichier = "mots.txt"  # Remplace par ton nom de fichier
nom_destination = "mots.txt"

supprimer_accents(nom_fichier, nom_destination)