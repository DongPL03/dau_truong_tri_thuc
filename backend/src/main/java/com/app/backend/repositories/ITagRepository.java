package com.app.backend.repositories;

import com.app.backend.models.Tag;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface ITagRepository extends JpaRepository<Tag, Long> {

    Optional<Tag> findBySlug(String slug);

    boolean existsBySlug(String slug);

    boolean existsByTen(String ten);

    List<Tag> findByHienThiTrueOrderByThuTuAscTenAsc();

    @Query("SELECT t FROM Tag t WHERE t.hienThi = true ORDER BY t.soBaiViet DESC")
    List<Tag> findPopularTags();

    @Query("SELECT t FROM Tag t WHERE t.hienThi = true ORDER BY t.soBaiViet DESC LIMIT :limit")
    List<Tag> findTopPopularTags(@Param("limit") int limit);

    @Query("SELECT t FROM Tag t WHERE LOWER(t.ten) LIKE LOWER(CONCAT('%', :keyword, '%')) AND t.hienThi = true")
    List<Tag> searchByName(@Param("keyword") String keyword);

    @Modifying
    @Query("UPDATE Tag t SET t.soBaiViet = t.soBaiViet + 1 WHERE t.id = :tagId")
    void incrementPostCount(@Param("tagId") Long tagId);

    @Modifying
    @Query("UPDATE Tag t SET t.soBaiViet = t.soBaiViet - 1 WHERE t.id = :tagId AND t.soBaiViet > 0")
    void decrementPostCount(@Param("tagId") Long tagId);

    List<Tag> findAllByIdIn(List<Long> ids);
}
