-- Chat System Tables
-- V4_0_0: Create chat room and message tables

-- Phong Chat (Chat Room)
CREATE TABLE IF NOT EXISTS phong_chat (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    ten VARCHAR(100),
    anh_nhom VARCHAR(255),
    loai ENUM('DON', 'NHOM') NOT NULL DEFAULT 'DON',
    tao_boi_id BIGINT,
    tao_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    cap_nhat_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    tin_nhan_cuoi VARCHAR(255),
    thoi_gian_tin_nhan_cuoi TIMESTAMP NULL,
    da_xoa BOOLEAN DEFAULT FALSE,
    
    CONSTRAINT fk_phong_chat_tao_boi FOREIGN KEY (tao_boi_id) REFERENCES nguoi_dung(id) ON DELETE SET NULL
);

-- Thanh Vien Phong Chat (Chat Room Members)
CREATE TABLE IF NOT EXISTS thanh_vien_phong_chat (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    phong_chat_id BIGINT NOT NULL,
    nguoi_dung_id BIGINT NOT NULL,
    vai_tro ENUM('ADMIN', 'THANH_VIEN') DEFAULT 'THANH_VIEN',
    biet_danh VARCHAR(50),
    tham_gia_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    doc_cuoi_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    da_tat_thong_bao BOOLEAN DEFAULT FALSE,
    da_ghim BOOLEAN DEFAULT FALSE,
    da_roi BOOLEAN DEFAULT FALSE,
    roi_luc TIMESTAMP NULL,
    
    CONSTRAINT fk_tv_phong_chat FOREIGN KEY (phong_chat_id) REFERENCES phong_chat(id) ON DELETE CASCADE,
    CONSTRAINT fk_tv_nguoi_dung FOREIGN KEY (nguoi_dung_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE,
    UNIQUE KEY uk_phong_chat_nguoi_dung (phong_chat_id, nguoi_dung_id)
);

-- Tin Nhan Phong Chat (Chat Messages)
CREATE TABLE IF NOT EXISTS tin_nhan_phong_chat (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    phong_chat_id BIGINT NOT NULL,
    gui_boi_id BIGINT NOT NULL,
    loai ENUM('VAN_BAN', 'HINH_ANH', 'TAP_TIN', 'AM_THANH', 'HE_THONG', 'STICKER', 'EMOJI') NOT NULL DEFAULT 'VAN_BAN',
    noi_dung TEXT,
    url_media VARCHAR(500),
    ten_file VARCHAR(255),
    kich_thuoc_file BIGINT,
    tra_loi_cho_id BIGINT,
    gui_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    chinh_sua_luc TIMESTAMP NULL,
    da_xoa BOOLEAN DEFAULT FALSE,
    xoa_luc TIMESTAMP NULL,
    da_ghim BOOLEAN DEFAULT FALSE,
    
    CONSTRAINT fk_tn_phong_chat FOREIGN KEY (phong_chat_id) REFERENCES phong_chat(id) ON DELETE CASCADE,
    CONSTRAINT fk_tn_gui_boi FOREIGN KEY (gui_boi_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE,
    CONSTRAINT fk_tn_tra_loi FOREIGN KEY (tra_loi_cho_id) REFERENCES tin_nhan_phong_chat(id) ON DELETE SET NULL,
    
    INDEX idx_tin_nhan_phong_chat (phong_chat_id, gui_luc DESC),
    INDEX idx_tin_nhan_gui_boi (gui_boi_id)
);

-- Create indexes for better performance
CREATE INDEX idx_phong_chat_tao_luc ON phong_chat(tao_luc DESC);
CREATE INDEX idx_phong_chat_cap_nhat ON phong_chat(cap_nhat_luc DESC);
CREATE INDEX idx_thanh_vien_nguoi_dung ON thanh_vien_phong_chat(nguoi_dung_id);
CREATE INDEX idx_thanh_vien_da_roi ON thanh_vien_phong_chat(da_roi);
