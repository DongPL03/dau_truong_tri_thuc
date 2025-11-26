# 🧩 HỆ THỐNG ĐẤU TRƯỜNG TRI THỨC — TỔNG HỢP TOÀN DIỆN (FULL FEATURES)

## 🎯 MỤC TIÊU

Xây dựng hệ thống web thi đấu trắc nghiệm nhiều người chơi (PvP) có chức năng xếp hạng, bạn bè, thống kê, và hệ thống quản trị.

---

## ⚙️ 1. CẤU TRÚC MODULE VÀ CHỨC NĂNG CHI TIẾT

| Mã                                                      | Module                               | Chức năng chính                                                                    | Người sử dụng |
| ------------------------------------------------------- | ------------------------------------ | ---------------------------------------------------------------------------------- | ------------- |
| **A. Authentication & User Management**                 |                                      |                                                                                    |               |
| A1                                                      | Đăng ký tài khoản                    | Username, email, password; mã hóa BCrypt; kiểm tra trùng lặp                       | User          |
| A2                                                      | Đăng nhập                            | JWT Auth; lưu refresh token; phân quyền ROLE_USER / ROLE_ADMIN                     | User          |
| A3                                                      | Cập nhật hồ sơ                       | Cập nhật tên hiển thị, avatar, email; upload ảnh                                   | User          |
| A4                                                      | Đổi mật khẩu                         | Kiểm tra mật khẩu cũ + xác nhận mật khẩu mới                                       | User          |
| A5                                                      | Quên mật khẩu / Reset                | (Tuỳ chọn sau) — Gửi email xác nhận                                                | User          |
| A6                                                      | Quản lý user                         | CRUD user, khóa tài khoản, reset mật khẩu, phân quyền                              | Admin         |
| **B. Quiz Management (User & Admin)**                   |                                      |                                                                                    |               |
| B1                                                      | Tạo bộ câu hỏi (bo_cau_hoi)          | Tiêu đề, mô tả, chủ đề, chế độ hiển thị (public/private)                           | User & Admin  |
| B2                                                      | Chỉnh sửa / Xóa bộ câu hỏi           | Chỉ người tạo hoặc Admin                                                           | User & Admin  |
| B3                                                      | Tạo câu hỏi (cau_hoi)                | Loại: Văn bản / Hình ảnh / Âm thanh / Video; 4 lựa chọn, 1 đáp án đúng, giải thích | User & Admin  |
| B4                                                      | Chỉnh sửa / Xóa câu hỏi              | CRUD nội dung câu hỏi                                                              | User & Admin  |
| B5                                                      | Hiển thị danh sách bộ câu hỏi        | Lọc theo chủ đề, độ khó, chế độ hiển thị                                           | User          |
| B6                                                      | Xem chi tiết bộ câu hỏi              | Gồm danh sách tất cả câu hỏi, độ khó, trạng thái duyệt                             | Admin         |
| B7                                                      | Duyệt / Từ chối bộ câu hỏi           | Với lý do từ chối + gắn nhãn "Official" nếu được duyệt                             | Admin         |
| B8                                                      | Duyệt / Chỉnh sửa từng câu hỏi       | ADMIN thao tác qua giao diện riêng (admin-question-editor)                         | Admin         |
| B9                                                      | Lưu bộ quiz chính thức               | Gắn nhãn `is_official = true` trong DB                                             | Admin         |
| B10                                                     | Xem / Luyện tập bộ quiz              | Chế độ practice; hiển thị câu hỏi, đúng/sai, giải thích                            | User          |
| **C. Battle System (Đấu Nhiều Người Chơi - Real-time)** |                                      |                                                                                    |               |
| C1                                                      | Tạo phòng đấu (Battle Room)          | Người chơi chọn quiz và chế độ công khai/riêng tư                                  | User          |
| C2                                                      | Tham gia phòng đấu                   | Tham gia qua mã phòng hoặc danh sách public room                                   | User          |
| C3                                                      | Đếm ngược và hiển thị câu hỏi        | 1 câu hỏi có bộ đếm 10s; hiển thị đáp án sau khi hết giờ                           | System        |
| C4                                                      | Chọn đáp án, tính điểm               | Điểm tính theo độ nhanh và chính xác                                               | System        |
| C5                                                      | Chat trong trận                      | Gửi tin nhắn thời gian thực                                                        | User          |
| C6                                                      | Hiển thị đáp án đúng/sai             | Hiệu ứng màu xanh/đỏ, animation điểm                                               | System        |
| C7                                                      | Kết thúc trận                        | Hiển thị bảng điểm tổng kết, top 3 người chơi                                      | System        |
| C8                                                      | Ghi lịch sử trận đấu                 | Lưu user_id, battle_id, điểm, số câu đúng/sai                                      | System        |
| C9                                                      | Quản lý phòng đấu                    | Admin xem, xóa trận bất thường                                                     | Admin         |
| **D. Leaderboard & Statistics**                         |                                      |                                                                                    |               |
| D1                                                      | Xếp hạng toàn cục                    | Tính theo tổng điểm tích lũy                                                       | User          |
| D2                                                      | Xếp hạng theo tuần / tháng           | Dựa trên battle_history                                                            | User          |
| D3                                                      | Xếp hạng theo chủ đề                 | Group by chu_de                                                                    | User          |
| D4                                                      | Thống kê tỷ lệ thắng / đúng          | Hiển thị trong profile người chơi                                                  | User          |
| D5                                                      | Admin Dashboard Chart.js             | Biểu đồ tăng trưởng user, top quiz, số trận, số quiz pending                       | Admin         |
| **E. Friend System & Chat**                             |                                      |                                                                                    |               |
| E1                                                      | Gửi lời mời kết bạn                  | Trạng thái: pending                                                                | User          |
| E2                                                      | Chấp nhận / Từ chối                  | Cập nhật trạng thái: accepted / rejected                                           | User          |
| E3                                                      | Hủy bạn bè                           | Xóa record trong bảng ban_be                                                       | User          |
| E4                                                      | Hiển thị trạng thái Online / Offline | Qua Redis / WebSocket                                                              | System        |
| E5                                                      | Tìm kiếm người dùng                  | Theo username hoặc email                                                           | User          |
| E6                                                      | Chat cơ bản (Private chat)           | (Tuỳ chọn sau) — Realtime message                                                  | User          |
| **F. Profile & Personal Dashboard**                     |                                      |                                                                                    |               |
| F1                                                      | Cập nhật thông tin cá nhân           | Avatar, display name, email                                                        | User          |
| F2                                                      | Đổi mật khẩu                         | Kiểm tra mật khẩu cũ                                                               | User          |
| F3                                                      | Xem lịch sử đấu                      | Các trận đã tham gia, điểm, thắng/thua                                             | User          |
| F4                                                      | Xem danh sách bạn bè                 | Lấy từ bảng ban_be                                                                 | User          |
| F5                                                      | Xóa tài khoản (Soft delete)          | User deactivate account                                                            | User          |
| **G. Admin Management Panel**                           |                                      |                                                                                    |               |
| G1                                                      | Dashboard tổng quan                  | KPI: tổng user, quiz, trận đấu, quiz chờ duyệt; biểu đồ Chart.js                   | Admin         |
| G2                                                      | Quản lý người dùng                   | Danh sách user, vai trò, trạng thái (active/blocked)                               | Admin         |
| G3                                                      | Quản lý quiz                         | Duyệt bộ câu hỏi, xem chi tiết, gắn “Official”                                     | Admin         |
| G4                                                      | Quản lý câu hỏi                      | Duyệt từng câu hỏi (approve/reject)                                                | Admin         |
| G5                                                      | Quản lý trận đấu                     | Xem lịch sử, xóa trận, thống kê                                                    | Admin         |
| G6                                                      | Báo cáo thống kê                     | Tỷ lệ hoạt động, số lượt chơi, top quiz phổ biến                                   | Admin         |
| G7                                                      | Phân quyền hệ thống                  | Thêm / xóa quyền, promote user lên admin                                           | Admin         |

---

## 🗃️ 2. SCHEMA DATABASE (MySQL 8)

```
-- ================================================
--  CƠ SỞ DỮ LIỆU: DAU_TRUONG_TRI_THUC
--  Phiên bản: v1.1 - Chuẩn hóa & tối ưu cho hệ thống Web Đấu Trường Tri Thức
--  Tác giả: Phạm Lê Đông & ChatGPT
-- ================================================]
-- drop schema dau_truong_tri_thuc
SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

CREATE DATABASE IF NOT EXISTS dau_truong_tri_thuc
  CHARACTER SET utf8mb4
  COLLATE utf8mb4_unicode_ci;

USE dau_truong_tri_thuc;

-- ================================================
-- 1.2. Bảng VAI_TRO
-- ================================================
CREATE TABLE vai_tro (
  id             BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  ten_vai_tro varchar(50)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;


-- ================================================
-- 1. Bảng NGUOI_DUNG
-- ================================================
CREATE TABLE nguoi_dung (
  id                BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  ten_dang_nhap     VARCHAR(50)  NOT NULL UNIQUE,
  email             VARCHAR(100) NOT NULL UNIQUE,
  mat_khau          VARCHAR(255) NOT NULL,
  ten_hien_thi      VARCHAR(100),
  avatar_url        VARCHAR(255),
  vai_tro_id        BIGINT UNSIGNED NOT NULL,
  FOREIGN KEY (vai_tro_id) REFERENCES vai_tro(id) ON DELETE RESTRICT ON UPDATE CASCADE,
  trang_thai        ENUM('ONLINE','OFFLINE','BANNED') DEFAULT 'OFFLINE',
  `is_active` tinyint(1) DEFAULT '1',
  last_login_at     DATETIME NULL,
  tao_luc           DATETIME DEFAULT CURRENT_TIMESTAMP,
  cap_nhat_luc      DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
-- alter table nguoi_dung add column `is_active` tinyint(1) DEFAULT '1'
-- alter table nguoi_dung add column `is_delete` tinyint(1) default '0'
-- alter table nguoi_dung drop column is_delete
-- alter table nguoi_dung add column `ho_ten` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT ''
-- alter table nguoi_dung add column `dia_chi` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT ''
CREATE INDEX idx_user_status ON nguoi_dung(trang_thai);

-- ================================================
-- 1.1. Bảng REFRESH_TOKEN
-- ================================================
-- CREATE TABLE refresh_token (
--   id             BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
--   nguoi_dung_id  BIGINT UNSIGNED NOT NULL,
--   token          VARCHAR(255) NOT NULL UNIQUE,
--   het_han_luc    DATETIME NOT NULL,
--   tao_luc        DATETIME DEFAULT CURRENT_TIMESTAMP,
--   FOREIGN KEY (nguoi_dung_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE
-- ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE `tokens` (
  `id` BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  `token` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
  `token_type` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
  `expiration_date` datetime DEFAULT NULL,
  `revoked` tinyint(1) NOT NULL,
  `expired` tinyint(1) NOT NULL,
  `nguoi_dung_id` BIGINT UNSIGNED DEFAULT NULL,
   FOREIGN KEY (`nguoi_dung_id`) REFERENCES `nguoi_dung` (`id`),
  `is_mobile` tinyint(1) DEFAULT '0',
  `refresh_token` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT '',
  `refresh_expiration_date` datetime DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

CREATE INDEX idx_token_user ON tokens(nguoi_dung_id);

-- ================================================
-- 2. Bảng CHU_DE
-- ================================================
CREATE TABLE chu_de (
  id        BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  ten       VARCHAR(100) NOT NULL UNIQUE,
  mo_ta     VARCHAR(255)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- ================================================
-- 3. Bảng BO_CAU_HOI
-- ================================================
CREATE TABLE bo_cau_hoi (
  id             BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  tieu_de        VARCHAR(100) NOT NULL,
  mo_ta          VARCHAR(255),
  chu_de_id      BIGINT UNSIGNED NOT NULL,
  tao_boi_id     BIGINT UNSIGNED NOT NULL,
  che_do_hien_thi ENUM('PUBLIC','PRIVATE') DEFAULT 'PUBLIC',
  trang_thai     ENUM('CHO_DUYET','DA_DUYET','TU_CHOI') DEFAULT 'DA_DUYET',
  ly_do_tu_choi  VARCHAR(255) NULL,
  tao_luc        DATETIME DEFAULT CURRENT_TIMESTAMP,
  cap_nhat_luc   DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  FOREIGN KEY (chu_de_id) REFERENCES chu_de(id) ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (tao_boi_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_bch_chude ON bo_cau_hoi(chu_de_id);
CREATE INDEX idx_bch_taoboi ON bo_cau_hoi(tao_boi_id);

-- ================================================
-- 4. Bảng CAU_HOI
-- ================================================
CREATE TABLE cau_hoi (
  id              BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  bo_cau_hoi_id   BIGINT UNSIGNED NOT NULL,
  do_kho          ENUM('DE','TRUNG_BINH','KHO') DEFAULT 'TRUNG_BINH',
  noi_dung        TEXT NOT NULL,
  loai_noi_dung   ENUM('VAN_BAN', 'HINH_ANH', 'AM_THANH', 'VIDEO') DEFAULT 'VAN_BAN',
  duong_dan_tep   VARCHAR(255) NULL,
  lua_chon_a      VARCHAR(255) NOT NULL,
  lua_chon_b      VARCHAR(255) NOT NULL,
  lua_chon_c      VARCHAR(255) NOT NULL,
  lua_chon_d      VARCHAR(255) NOT NULL,
  dap_an_dung     CHAR(1) NOT NULL,
  giai_thich      TEXT,
  cap_nhat_luc    DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  FOREIGN KEY (bo_cau_hoi_id) REFERENCES bo_cau_hoi(id) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT chk_dap_an CHECK (dap_an_dung IN ('A','B','C','D'))
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_cauhoi_bch ON cau_hoi(bo_cau_hoi_id);

-- ================================================
-- 5. Bảng TRAN_DAU
-- ================================================
CREATE TABLE tran_dau (
  id                BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  bo_cau_hoi_id     BIGINT UNSIGNED NOT NULL,
  chu_phong_id      BIGINT UNSIGNED NOT NULL,
  ma_phong          VARCHAR(10) NOT NULL UNIQUE,
  trang_thai        ENUM('PENDING','ONGOING','FINISHED') DEFAULT 'PENDING',
  cong_khai         BOOLEAN DEFAULT TRUE,
  ma_pin            VARCHAR(10) NULL,
  gioi_han_nguoi_choi INT DEFAULT 5,
  gioi_han_thoi_gian_cau_giay INT DEFAULT 15,
  luat_tinh_diem    ENUM('BASIC','SPEED_BONUS') DEFAULT 'SPEED_BONUS',
  winner_id         BIGINT UNSIGNED NULL,
  tao_luc           DATETIME DEFAULT CURRENT_TIMESTAMP,
  bat_dau_luc       DATETIME,
  ket_thuc_luc      DATETIME,
  FOREIGN KEY (bo_cau_hoi_id) REFERENCES bo_cau_hoi(id) ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (chu_phong_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (winner_id) REFERENCES nguoi_dung(id) ON DELETE SET NULL ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_trandau_bch ON tran_dau(bo_cau_hoi_id);

-- ================================================
-- 6. Bảng NGUOI_CHOI_TRAN_DAU
-- ================================================
CREATE TABLE nguoi_choi_tran_dau (
  id             BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  tran_dau_id    BIGINT UNSIGNED NOT NULL,
  nguoi_dung_id  BIGINT UNSIGNED NOT NULL,
  diem           INT DEFAULT 0,
  so_cau_dung    INT DEFAULT 0,
  xep_hang       INT,
  tham_gia_luc   DATETIME DEFAULT CURRENT_TIMESTAMP,
  UNIQUE (tran_dau_id, nguoi_dung_id),
  FOREIGN KEY (tran_dau_id) REFERENCES tran_dau(id) ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (nguoi_dung_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_nctd_trandau ON nguoi_choi_tran_dau(tran_dau_id);

-- ================================================
-- 7. Bảng TRA_LOI_TRAN_DAU
-- ================================================
CREATE TABLE tra_loi_tran_dau (
  id             BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  tran_dau_id    BIGINT UNSIGNED NOT NULL,
  nguoi_dung_id  BIGINT UNSIGNED NOT NULL,
  cau_hoi_id     BIGINT UNSIGNED NOT NULL,
  lua_chon       CHAR(1) NOT NULL,
  dung_hay_sai   BOOLEAN NOT NULL,
  thoi_gian_ms   INT,
  tra_loi_luc    DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (tran_dau_id) REFERENCES tran_dau(id) ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (nguoi_dung_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (cau_hoi_id) REFERENCES cau_hoi(id) ON DELETE RESTRICT ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_traloitd_trandau ON tra_loi_tran_dau(tran_dau_id);

-- ================================================
-- 8. Bảng LICH_SU_TRAN_DAU
-- ================================================
CREATE TABLE lich_su_tran_dau (
  id               BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  tran_dau_id      BIGINT UNSIGNED NOT NULL,
  nguoi_dung_id    BIGINT UNSIGNED NOT NULL,
  tong_diem        INT NOT NULL,
  tong_cau_dung    INT NOT NULL,
  tong_thoi_gian_ms INT,
  hoan_thanh_luc   DATETIME DEFAULT CURRENT_TIMESTAMP,
  UNIQUE (tran_dau_id, nguoi_dung_id),
  FOREIGN KEY (tran_dau_id) REFERENCES tran_dau(id) ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (nguoi_dung_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- ================================================
-- 9. Bảng TIN_NHAN
-- ================================================
CREATE TABLE tin_nhan (
  id            BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  tran_dau_id   BIGINT UNSIGNED NULL,
  gui_boi_id    BIGINT UNSIGNED NOT NULL,
  nhan_boi_id   BIGINT UNSIGNED NULL,
  noi_dung      TEXT NOT NULL,
  gui_luc       DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (tran_dau_id) REFERENCES tran_dau(id) ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (gui_boi_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (nhan_boi_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_tinnhan_nhanboi ON tin_nhan(nhan_boi_id);

-- ================================================
-- 10. Bảng KET_BAN
-- ================================================
CREATE TABLE ket_ban (
  id             BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  nguoi_gui_id   BIGINT UNSIGNED NOT NULL,
  nguoi_nhan_id  BIGINT UNSIGNED NOT NULL,
  trang_thai     ENUM('PENDING','ACCEPTED','DECLINED') DEFAULT 'PENDING',
  tao_luc        DATETIME DEFAULT CURRENT_TIMESTAMP,
  UNIQUE (nguoi_gui_id, nguoi_nhan_id),
  FOREIGN KEY (nguoi_gui_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (nguoi_nhan_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_kb_trangthai ON ket_ban(trang_thai);

-- ================================================
-- 11. Bảng THONG_BAO
-- ================================================
CREATE TABLE thong_bao (
  id            BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  nguoi_gui_id  BIGINT UNSIGNED NOT NULL,
  nguoi_nhan_id BIGINT UNSIGNED NOT NULL,
  loai          ENUM('FRIEND_REQUEST','BATTLE_INVITE','SYSTEM'),
  noi_dung      VARCHAR(255),
  metadata      JSON NULL,
  da_doc        BOOLEAN DEFAULT FALSE,
  tao_luc       DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (nguoi_gui_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (nguoi_nhan_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_tb_da_doc ON thong_bao(da_doc);
CREATE INDEX idx_tb_nhan ON thong_bao(nguoi_nhan_id);

-- ================================================
-- 12. Bảng BANG_XEP_HANG
-- ================================================
CREATE TABLE bang_xep_hang (
  id             BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  nguoi_dung_id  BIGINT UNSIGNED NOT NULL UNIQUE,
  tong_diem      INT DEFAULT 0,
  tong_tran      INT DEFAULT 0,
  xep_hang       INT,
  mua_giai_id    BIGINT UNSIGNED NULL,
  cap_nhat_luc   DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  FOREIGN KEY (nguoi_dung_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_bxh_diem ON bang_xep_hang(tong_diem);

-- ================================================
-- 13. Bảng PHIEN_LUYEN_TAP
-- ================================================
CREATE TABLE phien_luyen_tap (
  id              BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  nguoi_dung_id   BIGINT UNSIGNED NOT NULL,
  bo_cau_hoi_id   BIGINT UNSIGNED NOT NULL,
  tong_cau_hoi    INT NOT NULL,
  so_cau_dung     INT NOT NULL,
  do_chinh_xac    DECIMAL(5,2) NOT NULL,
  diem_so         INT DEFAULT 0,
  thoi_gian_tb_ms INT DEFAULT 0,
  tao_luc         DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (nguoi_dung_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (bo_cau_hoi_id) REFERENCES bo_cau_hoi(id) ON DELETE RESTRICT ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;


CREATE INDEX idx_pltap_user ON phien_luyen_tap(nguoi_dung_id);

-- ================================================
-- 14. Bảng TRA_LOI_LUYEN_TAP
-- ================================================
CREATE TABLE tra_loi_luyen_tap (
  id             BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  phien_id       BIGINT UNSIGNED NOT NULL,
  cau_hoi_id     BIGINT UNSIGNED NOT NULL,
  lua_chon       CHAR(1) NOT NULL,
  dung_hay_sai   BOOLEAN NOT NULL,
  thoi_gian_ms   INT,
  tra_loi_luc    DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (phien_id) REFERENCES phien_luyen_tap(id) ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (cau_hoi_id) REFERENCES cau_hoi(id) ON DELETE RESTRICT ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_tllt_phien ON tra_loi_luyen_tap(phien_id);

-- ================================================
-- 15. Bảng THE_GHI_NHO
-- ================================================
CREATE TABLE the_ghi_nho (
  id           BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  phien_id     BIGINT UNSIGNED NOT NULL,
  cau_hoi_id   BIGINT UNSIGNED NOT NULL,
  tao_luc      DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (phien_id) REFERENCES phien_luyen_tap(id) ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (cau_hoi_id) REFERENCES cau_hoi(id) ON DELETE RESTRICT ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

SET FOREIGN_KEY_CHECKS = 1;



-- ================================================
-- 16. Bảng VERIFY EMAIL
-- ================================================
CREATE TABLE email_verification_tokens (
  id BIGINT PRIMARY KEY AUTO_INCREMENT,
  token VARCHAR(100) NOT NULL UNIQUE,
  nguoi_dung_id BIGINT UNSIGNED NOT NULL,
  expires_at DATETIME NOT NULL,
  used_at DATETIME NULL,
  created_at DATETIME NOT NULL,
  CONSTRAINT fk_evt_user FOREIGN KEY (nguoi_dung_id) REFERENCES nguoi_dung(id)
);
CREATE INDEX idx_evt_user ON email_verification_tokens(nguoi_dung_id);
CREATE INDEX idx_evt_token ON email_verification_tokens(token);


-- ================================================
--  HOÀN TẤT DATABASE "ĐẤU TRƯỜNG TRI THỨC" (v1.1 FINAL)
-- ===============================================

-- 2️⃣ TỐI ƯU BẢNG tra_loi_tran_dau
-- 👉 Thêm UNIQUE để 1 người chỉ được trả lời 1 lần mỗi câu trong 1 trận
ALTER TABLE tra_loi_tran_dau
ADD CONSTRAINT uq_traloi_unique UNIQUE (tran_dau_id, nguoi_dung_id, cau_hoi_id);

-- 👉 Tăng tốc truy vấn điểm và kết quả trong trận
CREATE INDEX idx_traloi_dauhoi ON tra_loi_tran_dau(tran_dau_id, cau_hoi_id);
CREATE INDEX idx_traloi_user ON tra_loi_tran_dau(nguoi_dung_id);

-- 3️⃣ TỐI ƯU BẢNG tran_dau (tìm kiếm phòng nhanh hơn)
CREATE INDEX idx_trandau_trangthai_congkhai ON tran_dau(trang_thai, cong_khai);

-- 4️⃣ TỐI ƯU BẢNG cau_hoi (lọc câu hỏi theo độ khó)
CREATE INDEX idx_cauhoi_dokho ON cau_hoi(do_kho);

-- 5️⃣ TỐI ƯU BẢNG nguoi_choi_tran_dau (thường join với tran_dau + nguoi_dung)
CREATE INDEX idx_nctd_user ON nguoi_choi_tran_dau(nguoi_dung_id);

-- 6️⃣ TỐI ƯU BẢNG lich_su_tran_dau (thống kê nhanh)
CREATE INDEX idx_lst_user ON lich_su_tran_dau(nguoi_dung_id);

-- 7️⃣ BỔ SUNG KIỂM TRA TỰ ĐỘNG: CHECK đáp án (đề phòng mất constraint)
ALTER TABLE cau_hoi
ADD CONSTRAINT chk_dapan_dung CHECK (dap_an_dung IN ('A','B','C','D'));


-- INSERT DỮ LIỆU
INSERT INTO `vai_tro` (`id`, `ten_vai_tro`) VALUES
(1, 'user'),
(2, 'admin');

```

## 🔩 3. API BACKEND (Spring Boot)

### Authentication

```
POST /api/auth/register
POST /api/auth/login
POST /api/auth/refresh
GET /api/auth/profile
PUT /api/auth/profile/update
...còn nữa nếu có
```

### Quiz

```
GET /api/quiz?page=1&size=10
POST /api/quiz
GET /api/quiz/{id}
PUT /api/quiz/{id}
DELETE /api/quiz/{id}
...còn nữa nếu có
```

### Question

```
GET /api/questions?quizId=123
POST /api/questions
PUT /api/questions/{id}
DELETE /api/questions/{id}
...còn nữa nếu có
```

### Battle

```
POST /api/battles/create
POST /api/battles/join
GET /api/battles/{id}/status
POST /api/battles/{id}/answer
WS /battle/socket/{roomCode}
...còn nữa nếu có
```

### Leaderboard

```
GET /api/leaderboard/global
GET /api/leaderboard/weekly
GET /api/leaderboard/by-topic
...còn nữa nếu có
```

### Friends

```
POST /api/friends/add/{friendId}
POST /api/friends/accept/{friendId}
DELETE /api/friends/remove/{friendId}
GET /api/friends/list
...còn nữa nếu có
```

### Admin

```
GET /api/admin/users
PUT /api/admin/users/{id}/block
GET /api/admin/quiz/pending
PUT /api/admin/quiz/approve/{id}
DELETE /api/admin/quiz/reject/{id}
...còn nữa nếu có
```

---

## 🧠 4. NHỮNG ĐIỂM CẦN CHÚ Ý

### 🔐 Bảo mật

- Bắt buộc JWT + Refresh token.
- Mã hóa mật khẩu (BCrypt).
- Phân quyền rõ ràng theo vai trò (USER, ADMIN).
- CORS cấu hình cho FE–BE.

### ⚡ Hiệu năng

- Dùng Redis cache cho leaderboard, trạng thái online.
- Sử dụng pagination cho danh sách quiz, user, trận.
- Dùng WebSocket cho realtime battle.

### 🧾 Logging

- Log hành động quan trọng (login, tạo quiz, phê duyệt).
- Admin có trang xem logs.

### 📊 Thống kê

- Chart.js hoặc ECharts trên admin dashboard.(nếu được)
- Các biểu đồ: số người dùng, số trận, tỷ lệ quiz theo chủ đề.

### 🎨 UI/UX

- Màu chủ đạo: xanh lá `#4CAF50`, xanh dương `#2196F3`.
- Giao diện responsive.
- Animation nhẹ khi chọn đáp án, chuyển câu.
- Dùng SweetAlert2 hoặc Toastify cho alert và confirm.

---

## 📅 5. KẾ HOẠCH TRIỂN KHAI

| Giai đoạn | Hạng mục              | Kết quả                     |
| --------- | --------------------- | --------------------------- |
| Tuần 1–2  | Auth + User CRUD      | Đăng nhập/đăng ký hoạt động |
| Tuần 3–4  | Quiz + Question CRUD  | Tạo bộ câu hỏi và câu hỏi   |
| Tuần 5–6  | Battle Realtime       | WebSocket hoạt động         |
| Tuần 7–8  | Leaderboard + Friends | Hiển thị top, thêm bạn      |
| Tuần 9–10 | Admin Dashboard       | Duyệt nội dung, thống kê    |

---

## 📦 6. MÔI TRƯỜNG TRIỂN KHAI

### Backend

- Spring Boot 3.x
- MySQL 8
- Redis
- Docker Compose

### Frontend

- Angular 21
- TailwindCSS
- Bootstrap
- Chart.js
- Socket.IO client

---

## 🧩 7. CÁC MỞ RỘNG TƯƠNG LAI

- Chế độ thi tổ đội (2v2).
- Tích hợp ChatGPT cho chế độ luyện tập gợi ý đáp án.
- Hệ thống quest hàng ngày / nhiệm vụ.
- Xếp hạng theo khu vực hoặc trường học.
- API công khai cho developer bên thứ ba.

---

**Tác giả:** Phạm Lê Đông  
**Cập nhật:** 13/10/2025 13:27
