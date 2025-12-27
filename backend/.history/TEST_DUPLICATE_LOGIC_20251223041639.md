# Hướng dẫn Test Logic Duplicate Bộ Câu Hỏi

## 📋 Tổng quan
Logic duplicate cho phép admin tạo bản sao của một bộ câu hỏi (thường là của user) với 2 mục đích:
1. **COURSE**: Tạo bộ câu hỏi cho khóa học (COURSE_ONLY)
2. **RANKED**: Tạo bộ câu hỏi cho thi đấu ranked (RANKED_ONLY)

## 🔧 Endpoint

```
POST /api/v1/boCauHoi/{id}/duplicate?loai_su_dung={loai_su_dung}&purpose={purpose}
```

**Parameters:**
- `id` (path): ID của bộ câu hỏi gốc cần duplicate
- `loai_su_dung` (query): `COURSE_ONLY` hoặc `RANKED_ONLY`
- `purpose` (query): `COURSE` hoặc `RANKED`

**Headers:**
- `Authorization: Bearer {admin_token}`

## ✅ Test Cases

### Test Case 1: Duplicate cho COURSE
**Mục đích:** Tạo bộ câu hỏi COURSE_ONLY để thêm vào khóa học

**Request:**
```bash
curl -X POST "http://localhost:8088/api/v1/boCauHoi/1/duplicate?loai_su_dung=COURSE_ONLY&purpose=COURSE" \
  -H "Authorization: Bearer {admin_token}" \
  -H "Content-Type: application/json"
```

**Expected Results:**
1. ✅ Tạo bộ câu hỏi mới với:
   - `tieuDe` = "{tieuDe gốc} (Copy)"
   - `loaiSuDung` = `COURSE_ONLY`
   - `isOfficial` = `false`
   - `trangThai` = `DA_DUYET`
   - `taoBoi` = admin hiện tại
   - `canMoKhoa` = `false`
   - `giaMoKhoa` = `0`

2. ✅ Copy tất cả câu hỏi từ bộ gốc

3. ✅ Gửi thông báo broadcast cho toàn server:
   - `loai` = `SYSTEM`
   - `metadata.type` = `QUIZ_SELECTED_FOR_COURSE`
   - Nội dung: "Bộ câu hỏi \"{tieuDe}\" của {creatorName} đã được admin chọn làm bộ câu hỏi khóa học."

4. ✅ Unlock achievement `QUIZ_SELECTED_FOR_COURSE` cho creator (nếu chưa có)

5. ✅ Creator KHÔNG nhận gold/exp

---

### Test Case 2: Duplicate cho RANKED
**Mục đích:** Tạo bộ câu hỏi RANKED_ONLY cho thi đấu ranked

**Request:**
```bash
curl -X POST "http://localhost:8088/api/v1/boCauHoi/1/duplicate?loai_su_dung=RANKED_ONLY&purpose=RANKED" \
  -H "Authorization: Bearer {admin_token}" \
  -H "Content-Type: application/json"
```

**Expected Results:**
1. ✅ Tạo bộ câu hỏi mới với:
   - `tieuDe` = "{tieuDe gốc} (Copy)"
   - `loaiSuDung` = `RANKED_ONLY`
   - `isOfficial` = `true` ⭐
   - `trangThai` = `DA_DUYET`
   - `taoBoi` = admin hiện tại
   - `canMoKhoa` = `false`
   - `giaMoKhoa` = `0`

2. ✅ Copy tất cả câu hỏi từ bộ gốc

3. ✅ Gửi thông báo broadcast cho toàn server:
   - `loai` = `SYSTEM`
   - `metadata.type` = `QUIZ_SELECTED_FOR_RANKED`
   - Nội dung: "Bộ câu hỏi \"{tieuDe}\" của {creatorName} đã được admin chọn làm bộ câu hỏi thi đấu ranked chính thức."

4. ✅ Tặng phần thưởng cho creator:
   - +200 gold
   - +100 exp (tongXp)

5. ✅ Unlock achievement `QUIZ_SELECTED_FOR_RANKED` cho creator (nếu chưa có)

---

### Test Case 3: Error - Không phải admin
**Request:**
```bash
curl -X POST "http://localhost:8088/api/v1/boCauHoi/1/duplicate?loai_su_dung=COURSE_ONLY&purpose=COURSE" \
  -H "Authorization: Bearer {user_token}" \
  -H "Content-Type: application/json"
```

**Expected Result:**
- ❌ Status: 403 Forbidden
- ❌ Message: "Chỉ admin mới có thể duplicate bộ câu hỏi"

---

### Test Case 4: Error - Bộ câu hỏi không tồn tại
**Request:**
```bash
curl -X POST "http://localhost:8088/api/v1/boCauHoi/99999/duplicate?loai_su_dung=COURSE_ONLY&purpose=COURSE" \
  -H "Authorization: Bearer {admin_token}" \
  -H "Content-Type: application/json"
```

**Expected Result:**
- ❌ Status: 404 Not Found
- ❌ Message: "Bộ câu hỏi không tồn tại"

---

### Test Case 5: Error - Tham số không hợp lệ
**Request:**
```bash
curl -X POST "http://localhost:8088/api/v1/boCauHoi/1/duplicate?loai_su_dung=INVALID&purpose=COURSE" \
  -H "Authorization: Bearer {admin_token}" \
  -H "Content-Type: application/json"
```

**Expected Result:**
- ❌ Status: 400 Bad Request
- ❌ Message: "loaiSuDung phải là COURSE_ONLY hoặc RANKED_ONLY"

---

## 🧪 Test qua Frontend

1. **Đăng nhập với tài khoản admin**
2. **Vào trang chi tiết bộ câu hỏi** (của user hoặc admin)
3. **Click nút "Duplicate"**
4. **Chọn purpose:**
   - **COURSE**: Để tạo bộ câu hỏi cho khóa học
   - **RANKED**: Để tạo bộ câu hỏi cho ranked
5. **Click "Duplicate"**
6. **Kiểm tra:**
   - ✅ Điều hướng đến bộ câu hỏi mới
   - ✅ Kiểm tra thông tin bộ câu hỏi mới
   - ✅ Kiểm tra thông báo trong notification bell
   - ✅ Kiểm tra achievement của creator (nếu là RANKED)
   - ✅ Kiểm tra gold/exp của creator (nếu là RANKED)

---

## 📊 Kiểm tra Database

### Sau khi duplicate COURSE:
```sql
-- Kiểm tra bộ câu hỏi mới
SELECT id, tieu_de, loai_su_dung, is_chinh_thuc, trang_thai, tao_boi_id 
FROM bo_cau_hoi 
WHERE tieu_de LIKE '%(Copy)%' 
ORDER BY tao_luc DESC LIMIT 1;

-- Kiểm tra số câu hỏi
SELECT COUNT(*) 
FROM cau_hoi 
WHERE bo_cau_hoi_id = {id_bo_cau_hoi_moi};

-- Kiểm tra thông báo broadcast
SELECT COUNT(*) 
FROM thong_bao 
WHERE metadata LIKE '%QUIZ_SELECTED_FOR_COURSE%' 
AND tao_luc >= NOW() - INTERVAL 1 MINUTE;

-- Kiểm tra achievement
SELECT * 
FROM nguoi_dung_thanh_tich 
WHERE nguoi_dung_id = {creator_id} 
AND code = 'QUIZ_SELECTED_FOR_COURSE';
```

### Sau khi duplicate RANKED:
```sql
-- Kiểm tra bộ câu hỏi mới
SELECT id, tieu_de, loai_su_dung, is_chinh_thuc, trang_thai, tao_boi_id 
FROM bo_cau_hoi 
WHERE tieu_de LIKE '%(Copy)%' 
ORDER BY tao_luc DESC LIMIT 1;

-- Kiểm tra gold/exp của creator
SELECT tien_vang, tong_xp 
FROM bang_xep_hang 
WHERE nguoi_dung_id = {creator_id};

-- Kiểm tra thông báo broadcast
SELECT COUNT(*) 
FROM thong_bao 
WHERE metadata LIKE '%QUIZ_SELECTED_FOR_RANKED%' 
AND tao_luc >= NOW() - INTERVAL 1 MINUTE;

-- Kiểm tra achievement
SELECT * 
FROM nguoi_dung_thanh_tich 
WHERE nguoi_dung_id = {creator_id} 
AND code = 'QUIZ_SELECTED_FOR_RANKED';
```

---

## 🔍 Checklist Test

### Test Case COURSE:
- [ ] Bộ câu hỏi mới được tạo với đúng thông tin
- [ ] `loaiSuDung` = `COURSE_ONLY`
- [ ] `isOfficial` = `false`
- [ ] Tất cả câu hỏi được copy
- [ ] Thông báo broadcast được gửi cho tất cả users
- [ ] Achievement `QUIZ_SELECTED_FOR_COURSE` được unlock cho creator
- [ ] Creator KHÔNG nhận gold/exp

### Test Case RANKED:
- [ ] Bộ câu hỏi mới được tạo với đúng thông tin
- [ ] `loaiSuDung` = `RANKED_ONLY`
- [ ] `isOfficial` = `true`
- [ ] Tất cả câu hỏi được copy
- [ ] Thông báo broadcast được gửi cho tất cả users
- [ ] Creator nhận +200 gold
- [ ] Creator nhận +100 exp
- [ ] Achievement `QUIZ_SELECTED_FOR_RANKED` được unlock cho creator

### Test Case Error:
- [ ] User không phải admin không thể duplicate
- [ ] Bộ câu hỏi không tồn tại trả về 404
- [ ] Tham số không hợp lệ trả về 400

---

## 💡 Tips

1. **Test với bộ câu hỏi có nhiều câu hỏi** để đảm bảo tất cả được copy
2. **Test với creator là user thường** để kiểm tra gold/exp và achievement
3. **Kiểm tra notification bell** để xem thông báo broadcast
4. **Kiểm tra profile của creator** để xem achievement và gold/exp
5. **Test duplicate nhiều lần** để đảm bảo không bị duplicate achievement

