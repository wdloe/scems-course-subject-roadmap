#!/usr/bin/env python3
"""Crawl all 2026 Bachelor/Master programs listed under IT and engineering."""

from __future__ import annotations

import argparse
import csv
import json
from datetime import datetime, timezone
from pathlib import Path

from crawl_latrobe import (
    BASE,
    aos_record,
    course_record,
    download,
    page_content,
    subject_record,
    walk_items,
    enforce_year,
)


# Discovered from the handbook's IT and engineering discipline browse page.
COURSE_CODES = [
    # Bachelor programs
    "SHCEB", "SHCE", "LZCCS", "TB002IB", "SBCS", "TB005", "TB005SP", "TB005TS",
    "SBCY", "TB001O", "TB004SP", "TB003SY", "SZCYC", "SZCYCR", "SZCYPS",
    "SHENIB", "SHENI", "RBC", "SBIT", "SBITO", "SBITSD",
    # Master programs
    "TM010", "TM010O", "TM011", "TM011O", "TM011SY", "TM013", "SMAI", "SMAIO",
    "LMBAN", "BM007", "BM014", "BM005", "BM015", "TM005", "SMCEMB", "SMCEM",
    "TM009", "TM003", "TM003O", "SMCYB", "TM014", "SMDS", "SMDSO", "TM001B",
    "TM001", "LMEM", "SMINCT", "TM015", "SMITB", "SMIT", "SMITO", "TM007SY",
    "AMIRL", "AM003O", "SMIOTB", "TM006", "TM012B", "TM012", "TM004",
]


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--year", type=int, default=2026)
    ap.add_argument("--output", type=Path, default=Path("outputs/latrobe-it-engineering-2026-active"))
    ap.add_argument("--delay", type=float, default=0.15)
    args = ap.parse_args()
    out, raw = args.output, args.output / "raw"
    out.mkdir(parents=True, exist_ok=True)

    courses, majors_by_code, subject_codes, errors = [], {}, set(), []
    for code in COURSE_CODES:
        url = f"{BASE}/courses/{args.year}/{code}"
        try:
            body = download(url, raw / "courses" / f"{code}.html", args.delay)
            content = page_content(body, url)
            course = course_record(content, args.year, url)
            if any(content.get(flag) for flag in ("is_phasing_out", "is_discontinued", "is_suspended", "is_closed")):
                errors.append({"entity": "course", "code": code, "url": url, "error": "excluded: phasing out/discontinued/suspended/closed"})
                continue
            enforce_year(course.get("structure"), args.year)
            courses.append(course)
            for item in walk_items(course.get("structure")):
                if item.get("type") == "subject" and item.get("code"):
                    subject_codes.add(item["code"])
            # Course pages include direct AOS links, even when not represented as a tree item.
            import re
            for major_code in sorted(set(re.findall(rf"/aos/{args.year}/([A-Za-z0-9-]+)", body, re.I))):
                majors_by_code.setdefault(major_code, None)
        except Exception as exc:
            errors.append({"entity": "course", "code": code, "url": url, "error": str(exc)})

    majors = []
    for code in sorted(majors_by_code):
        url = f"{BASE}/aos/{args.year}/{code}"
        try:
            body = download(url, raw / "areas-of-study" / f"{code}.html", args.delay)
            major_content = page_content(body, url)
            major = aos_record(major_content, args.year, url)
            if any(major_content.get(flag) for flag in ("is_phasing_out", "is_discontinued", "is_suspended", "is_closed")):
                errors.append({"entity": "major", "code": code, "url": url, "error": "excluded: phasing out/discontinued/suspended/closed"})
                continue
            enforce_year(major.get("structure"), args.year)
            majors.append(major)
            for item in walk_items(major.get("structure")):
                if item.get("type") == "subject" and item.get("code"):
                    subject_codes.add(item["code"])
        except Exception as exc:
            errors.append({"entity": "major", "code": code, "url": url, "error": str(exc)})

    subjects = []
    for code in sorted(subject_codes):
        url = f"{BASE}/subjects/{args.year}/{code}"
        try:
            body = download(url, raw / "subjects" / f"{code}.html", args.delay)
            subjects.append(subject_record(page_content(body, url), args.year, url))
        except Exception as exc:
            errors.append({"entity": "subject", "code": code, "url": url, "error": str(exc)})

    crawled_at = datetime.now(timezone.utc).isoformat()
    data = {
        "metadata": {
            "academic_year": args.year,
            "discipline": "IT and engineering",
            "crawled_at": crawled_at,
            "course_count_requested": len(COURSE_CODES),
            "course_count_downloaded": len(courses),
            "major_count": len(majors),
            "subject_count": len(subjects),
        },
        "courses": courses,
        "majors": majors,
        "subjects": subjects,
    }
    (out / "handbook-data.json").write_text(json.dumps(data, indent=2, ensure_ascii=False), encoding="utf-8")
    if subjects:
        fields = [k for k in subjects[0] if k != "requisites_raw"]
        with (out / "subjects.csv").open("w", newline="", encoding="utf-8") as fh:
            writer = csv.DictWriter(fh, fieldnames=fields)
            writer.writeheader()
            writer.writerows({k: s.get(k) for k in fields} for s in subjects)
    summary = {
        **data["metadata"],
        "requested_course_codes": COURSE_CODES,
        "download_errors": errors,
        "source": "https://handbook.latrobe.edu.au/browse/By%20Discipline/ITand%20engineering",
    }
    (out / "crawl-summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    print(json.dumps(summary, indent=2))


if __name__ == "__main__":
    main()
