package com.app.backend.repositories;

import com.app.backend.models.HinhAnhBaiViet;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface IHinhAnhBaiVietRepository extends JpaRepository<HinhAnhBaiViet, Long> {

    List<HinhAnhBaiViet> findByBaiViet_IdOrderByThuTuAsc(Long baiVietId);

    void deleteAllByBaiViet_Id(Long baiVietId);

    long countByBaiViet_Id(Long baiVietId);
}
