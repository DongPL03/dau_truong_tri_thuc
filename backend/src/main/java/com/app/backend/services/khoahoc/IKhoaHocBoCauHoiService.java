package com.app.backend.services.khoahoc;

import com.app.backend.dtos.AddBoCauHoiToKhoaHocDTO;
import com.app.backend.dtos.UpdateBoCauHoiInKhoaHocDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.KhoaHocBoCauHoi;

public interface IKhoaHocBoCauHoiService {
    /**
     * Thêm bộ câu hỏi vào khóa học
     */
    KhoaHocBoCauHoi addBoCauHoiToKhoaHoc(Long khoaHocId, AddBoCauHoiToKhoaHocDTO dto, Long adminId)
            throws DataNotFoundException, PermissionDenyException;

    /**
     * Cập nhật thông tin bộ câu hỏi trong khóa học (thứ tự, điểm tối thiểu, bắt buộc)
     */
    KhoaHocBoCauHoi updateBoCauHoiInKhoaHoc(Long khoaHocId, Long boCauHoiId, UpdateBoCauHoiInKhoaHocDTO dto, Long adminId)
            throws DataNotFoundException, PermissionDenyException;

    /**
     * Xóa bộ câu hỏi khỏi khóa học
     */
    void removeBoCauHoiFromKhoaHoc(Long khoaHocId, Long boCauHoiId, Long adminId)
            throws DataNotFoundException, PermissionDenyException;
}

