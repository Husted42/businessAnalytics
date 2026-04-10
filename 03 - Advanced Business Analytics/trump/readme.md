# Scraping
### Prequsits
**Truthbrush**

```
sudo apt update
sudo apt install python3-pip -y

pip3 install truthbrush

export TRUTHSOCIAL_USERNAME="your_username"
export TRUTHSOCIAL_PASSWORD="your_password"

truthbrush statuses realDonaldTrump > realDonaldTrump_truths.json
```