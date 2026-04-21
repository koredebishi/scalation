"""Convert the PhD progress report Markdown to PDF using fpdf2 (pure Python, no system deps)."""
import re
from fpdf import FPDF

INPUT  = r'C:\Simulation\scalation_2.0\docs\PhD_Student_Progress_Self_Reporting_Spring2026_DRAFT.md'
OUTPUT = r'C:\Simulation\scalation_2.0\docs\PhD_Student_Progress_Self_Reporting_Spring2026_DRAFT.pdf'

class ReportPDF(FPDF):
    def header(self):
        pass
    def footer(self):
        self.set_y(-15)
        self.set_font('Times', 'I', 9)
        self.cell(0, 10, f'Page {self.page_no()}/{{nb}}', align='C')

pdf = ReportPDF()
pdf.alias_nb_pages()
pdf.set_auto_page_break(auto=True, margin=25)
pdf.add_page()

with open(INPUT, 'r', encoding='utf-8') as f:
    lines = f.readlines()

def clean(text):
    """Strip markdown bold/italic markers for plain text output."""
    text = text.replace('**', '').replace('__', '')
    text = re.sub(r'\*([^*]+)\*', r'\1', text)
    text = re.sub(r'_([^_]+)_', r'\1', text)
    # Replace Unicode chars that Times (Latin-1) can't handle
    text = text.replace('\u2014', '--')   # em-dash
    text = text.replace('\u2013', '-')    # en-dash
    text = text.replace('\u2018', "'")    # left single quote
    text = text.replace('\u2019', "'")    # right single quote
    text = text.replace('\u201c', '"')    # left double quote
    text = text.replace('\u201d', '"')    # right double quote
    text = text.replace('\u2022', '-')    # bullet
    text = text.replace('\u2026', '...')  # ellipsis
    text = text.replace('\u00d7', 'x')    # multiplication sign
    text = text.replace('\u2248', '~')    # approx
    return text.strip()

in_table = False
table_rows = []

def flush_table(pdf, rows):
    if not rows:
        return
    n_cols = len(rows[0])
    col_w = (pdf.w - pdf.l_margin - pdf.r_margin) / n_cols
    pdf.set_font('Times', 'B', 9)
    for cell in rows[0]:
        pdf.cell(col_w, 6, clean(cell), border=1, align='C')
    pdf.ln()
    pdf.set_font('Times', '', 9)
    for row in rows[1:]:
        for i, cell in enumerate(row):
            pdf.cell(col_w, 6, clean(cell), border=1)
        pdf.ln()
    pdf.ln(3)

i = 0
while i < len(lines):
    line = lines[i].rstrip('\n')

    # Table detection
    if '|' in line and line.strip().startswith('|'):
        cells = [c.strip() for c in line.split('|')[1:-1]]
        # Skip separator rows (e.g., |---|---|)
        if all(re.match(r'^[-:]+$', c) for c in cells):
            i += 1
            continue
        if not in_table:
            in_table = True
            table_rows = []
        table_rows.append(cells)
        i += 1
        continue
    else:
        if in_table:
            flush_table(pdf, table_rows)
            in_table = False
            table_rows = []

    # Heading 1
    if line.startswith('# ') and not line.startswith('## '):
        pdf.set_font('Times', 'B', 16)
        pdf.cell(0, 10, clean(line[2:]), align='C', new_x="LMARGIN", new_y="NEXT")
        pdf.ln(2)
        i += 1
        continue

    # Heading 2
    if line.startswith('## '):
        pdf.ln(4)
        pdf.set_font('Times', 'B', 13)
        pdf.cell(0, 8, clean(line[3:]), new_x="LMARGIN", new_y="NEXT")
        # Draw underline
        pdf.line(pdf.l_margin, pdf.get_y(), pdf.w - pdf.r_margin, pdf.get_y())
        pdf.ln(3)
        i += 1
        continue

    # Heading 3
    if line.startswith('### '):
        pdf.ln(2)
        pdf.set_font('Times', 'B', 12)
        pdf.cell(0, 7, clean(line[4:]), new_x="LMARGIN", new_y="NEXT")
        pdf.ln(1)
        i += 1
        continue

    # Horizontal rule
    if line.strip() == '---':
        pdf.ln(3)
        pdf.line(pdf.l_margin, pdf.get_y(), pdf.w - pdf.r_margin, pdf.get_y())
        pdf.ln(5)
        i += 1
        continue

    # Bullet list
    if line.strip().startswith('- '):
        pdf.set_font('Times', '', 11)
        text = clean(line.strip()[2:])
        x = pdf.l_margin + 8
        pdf.set_x(x)
        pdf.cell(5, 6, '-')  # bullet
        pdf.multi_cell(pdf.w - pdf.r_margin - x - 5, 6, text)
        i += 1
        continue

    # Bold field line (e.g., **Field:** Value)
    if line.strip().startswith('**') and ':**' in line:
        pdf.set_font('Times', '', 11)
        text = clean(line)
        pdf.multi_cell(0, 6, text)
        i += 1
        continue

    # Empty line
    if line.strip() == '':
        pdf.ln(2)
        i += 1
        continue

    # Regular paragraph
    pdf.set_font('Times', '', 11)
    pdf.multi_cell(0, 6, clean(line))
    i += 1

# Flush any trailing table
if in_table:
    flush_table(pdf, table_rows)

pdf.output(OUTPUT)
print(f'PDF saved to: {OUTPUT}')
