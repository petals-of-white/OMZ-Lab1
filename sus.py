# Припустимо, що image_data - це 32-бітне ціле число, яке представляє піксель
# Кожний біт в цьому числі має своє значення: 0bARGB (A - альфа, R - червоний, G - зелений, B - синій)

image_data = 0x11111111
alpha = 0b10101011
red = 0b11001101
green = 0b11101111
blue = 0b00000001

# Комбінуємо байти в 32-бітне ціле число
image_data = (alpha << 24) | (red << 16) | (green << 8) | blue
# Запис значень кожного з колірних каналів
alpha_channel = (image_data >> 24) & 0xFF
red_channel = (image_data >> 16) & 0xFF
green_channel = (image_data >> 8) & 0xFF
blue_channel = image_data & 0xFF

# Вивід значень кожного з колірних каналів
print(f"Alpha: {alpha_channel}, Red: {red_channel}, Green: {green_channel}, Blue: {blue_channel}")