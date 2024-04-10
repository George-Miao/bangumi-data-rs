use std::{collections::BTreeMap, fmt::Display, marker::PhantomData, str::FromStr};

use iso8601::DateTime;
use nom::{branch::alt, bytes::complete::tag, combinator::map, sequence::tuple, IResult};
use serde::{de::Visitor, Deserialize, Deserializer, Serialize};

const ROOT: &str = "https://github.com/bangumi-data/bangumi-data/raw/master";
const DIST: &str = "https://unpkg.com/bangumi-data@0.3/dist/data.json";

#[cfg(feature = "reqwest")]
pub async fn get_all() -> Result<BangumiData, reqwest::Error> {
    reqwest::get(DIST)
        .await?
        .json()
        .await
}

#[cfg(feature = "reqwest")]
pub async fn get_by_month(year: u32, month: u8) -> Result<Vec<Item>, reqwest::Error> {
    assert!(month <= 12);
    assert!(year >= 1960);

    reqwest::get(format!("{ROOT}/data/items/{year}/{month:02}.json"))
        .await?
        .json()
        .await
}

#[cfg(feature = "reqwest")]
pub async fn get_info_site() -> Result<BTreeMap<String, SiteMeta>, reqwest::Error> {
    reqwest::get(format!("{ROOT}/data/sites/info.json"))
        .await?
        .json()
        .await
}

#[cfg(feature = "reqwest")]
pub async fn get_on_air_site() -> Result<BTreeMap<String, SiteMeta>, reqwest::Error> {
    reqwest::get(format!("{ROOT}/data/sites/onair.json"))
        .await?
        .json()
        .await
}

#[cfg(feature = "reqwest")]
pub async fn get_resource_site() -> Result<BTreeMap<String, SiteMeta>, reqwest::Error> {
    reqwest::get(format!("{ROOT}/data/sites/resource.json"))
        .await?
        .json()
        .await
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "ts", derive(ts_rs::TS))]
#[serde(rename_all = "camelCase")]
pub struct BangumiData {
    pub site_meta: BTreeMap<String, SiteMeta>,
    pub items: Vec<Item>,
}

impl FromStr for BangumiData {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl BangumiData {
    pub fn from_bytes(s: &[u8]) -> Result<Self, serde_json::Error> {
        serde_json::from_slice(s)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "ts", derive(ts_rs::TS))]
#[serde(rename_all = "camelCase")]
pub struct SiteMeta {
    pub title: String,
    pub url_template: String,
    #[serde(rename = "type", deserialize_with = "empty_str", default)]
    pub site_type: Option<String>,
    pub regions: Option<Vec<String>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "ts", derive(ts_rs::TS))]
#[serde(rename_all = "lowercase")]
#[non_exhaustive]
pub enum ItemType {
    TV,
    Web,
    Ova,
    Movie,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "ts", derive(ts_rs::TS))]
#[serde(rename_all = "camelCase")]
pub struct Item {
    pub title: String,
    pub title_translate: BTreeMap<Language, Vec<String>>,
    #[serde(rename = "type")]
    pub item_type: ItemType,
    pub lang: Language,
    pub official_site: String,
    #[serde(deserialize_with = "empty_str", default)]
    #[cfg_attr(feature = "ts", ts(type = "Option<String>"))]
    pub begin: Option<DateTime>,
    #[serde(deserialize_with = "empty_str", default)]
    #[cfg_attr(feature = "ts", ts(type = "Option<String>"))]
    pub end: Option<DateTime>,
    pub sites: Vec<Site>,
    #[serde(deserialize_with = "empty_str", default)]
    pub broadcast: Option<Broadcast>,
    #[serde(deserialize_with = "empty_str", default)]
    pub comment: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "ts", derive(ts_rs::TS))]
#[serde(rename_all = "camelCase")]
pub struct Site {
    pub site: String,
    #[serde(deserialize_with = "empty_str", default)]
    pub id: Option<String>,
    #[serde(deserialize_with = "empty_str", default)]
    pub begin: Option<String>,
    #[serde(deserialize_with = "empty_str", default)]
    pub broadcast: Option<String>,
    #[serde(deserialize_with = "empty_str", default)]
    pub comment: Option<String>,
    #[serde(deserialize_with = "empty_str", default)]
    pub url: Option<String>,
    pub regions: Option<Vec<String>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "ts", derive(ts_rs::TS))]
pub struct Broadcast {
    #[cfg_attr(feature = "ts", ts(type = "String"))]
    pub begin: DateTime,
    #[cfg_attr(feature = "ts", ts(type = "String"))]
    pub period: Period,
}

impl FromStr for Broadcast {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse(s.as_bytes())
            .map_err(|e| format!("Unable to parse broadcast: {e}"))
            .map(|(_, b)| b)
    }
}

impl<'de> Deserialize<'de> for Broadcast {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        Broadcast::from_str(&s).map_err(serde::de::Error::custom)
    }
}

impl Display for Broadcast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let period = match self.period {
            Period::Once => "0D",
            Period::Daily => "1D",
            Period::Weekly => "7D",
            Period::Monthly => "1M",
        };
        write!(f, "R/{}/P{}", self.begin, period)
    }
}

impl Serialize for Broadcast {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_str(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "ts", derive(ts_rs::TS))]
pub enum Period {
    Once,
    Daily,
    Weekly,
    Monthly,
}

fn empty_str<'de, T, D>(de: D) -> Result<Option<T>, D::Error>
where
    T: FromStr + 'de,
    T::Err: Display,
    D: Deserializer<'de>,
{
    struct EmptyStringVisitor<'de, T>(PhantomData<&'de T>);

    impl<'de, T> Visitor<'de> for EmptyStringVisitor<'de, T>
    where
        T: FromStr,
        T::Err: Display,
    {
        type Value = Option<T>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a string")
        }

        fn visit_unit<E>(self) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(None)
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            if v.is_empty() {
                return Ok(None);
            }
            T::from_str(v).map_err(serde::de::Error::custom).map(Some)
        }
    }

    de.deserialize_any(EmptyStringVisitor(PhantomData))
}

fn parse_period(input: &[u8]) -> IResult<&[u8], Period> {
    alt((
        map(tag("0D"), |_| Period::Once),
        map(tag("1D"), |_| Period::Daily),
        map(tag("7D"), |_| Period::Weekly),
        map(tag("1M"), |_| Period::Monthly),
    ))(input)
}

fn parse(input: &[u8]) -> IResult<&[u8], Broadcast> {
    map(
        tuple((
            tag("R/"),
            iso8601::parsers::parse_datetime,
            tag("/P"),
            parse_period,
        )),
        |(_, begin, _, period)| Broadcast { begin, period },
    )(input)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[cfg_attr(feature = "ts", derive(ts_rs::TS))]
#[non_exhaustive]
pub enum Language {
    #[serde(rename = "zh-Hans")]
    ZhHans,
    #[serde(rename = "zh-Hant")]
    ZhHant,
    #[serde(rename = "en")]
    En,
    #[serde(rename = "ja")]
    Ja,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_broadcast() {
        const BYTES: &[u8] = b"R/2020-01-01T13:00:00Z/P0D";
        let (_, a) = parse(BYTES).unwrap();
        assert_eq!(a.period, Period::Once);

        let a_str = a.to_string();
        println!("{}", a_str);
        assert_eq!(BYTES, a_str.as_bytes());
        let (_, b) = parse(a_str.as_bytes()).unwrap();

        assert_eq!(a, b);
    }

    #[test]
    fn test_datetime() {
        let time = iso8601::datetime("2020-01-01T13:00:00Z").unwrap();
        let (_, b) = parse(b"R/2020-01-01T13:00:00Z/P0D").unwrap();
        assert_eq!(b.period, Period::Once);
        assert_eq!(b.begin, time);
        let (_, b) = parse(b"R/2020-01-01T13:00:00Z/P1D").unwrap();
        assert_eq!(b.period, Period::Daily);
        let (_, b) = parse(b"R/2020-01-01T13:00:00Z/P7D").unwrap();
        assert_eq!(b.period, Period::Weekly);
        let (_, b) = parse(b"R/2020-01-01T13:00:00Z/P1M").unwrap();
        assert_eq!(b.period, Period::Monthly);
    }

    #[test]
    fn local() {
        let s = std::fs::read_to_string("data/dist.json").unwrap();
        let b = BangumiData::from_str(&s).unwrap();
        println!("{b:#?}",)
    }

    #[tokio::test]
    async fn remote() {
        println!("{:#?}\n============", get_all().await.unwrap());
        println!("{:#?}\n============", get_by_month(2023, 10).await.unwrap());
        println!("{:#?}\n============", get_info_site().await.unwrap());
        println!("{:#?}\n============", get_on_air_site().await.unwrap());
        println!("{:#?}\n============", get_resource_site().await.unwrap());
    }
}
