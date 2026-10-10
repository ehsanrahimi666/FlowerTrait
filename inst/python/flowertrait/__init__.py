"""flowertrait: colour, pattern and shape traits of flowers from photographs (back end of the FlowerTrait R package).

The core (traits from an image + outline) depends only on numpy, OpenCV and pandas, so it runs on a laptop and
from R (reticulate).

Photo pipeline:
    photo.py        photo -> Grounding DINO-base (two stages) -> native crop -> SAM 2.1-B outline -> QC -> core traits
    shape.py        radial colour pattern, symmetry and petal number (reproduced exactly in R)
    photofilter.py  is the photo usable? face view, open flower, whole flower visible, sharp
    run_photos.py   batch CLI: folder or CSV of URLs -> traits.csv, crops/, masks/
These need torch, transformers, ultralytics and open_clip_torch (heavy imports are kept inside those modules).
"""
__version__ = "0.1.0"

from .segment import load_image, segment, quality_metrics  # noqa: F401
from .colour import colour_metrics, colour_name, ciede2000  # noqa: F401
from .pattern import pattern_metrics, shape_metrics  # noqa: F401
from .shape import radial_metrics  # noqa: F401
from .pipeline import extract_traits, process_image  # noqa: F401
