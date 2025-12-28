package com.app.backend.services.community;

import com.app.backend.dtos.community.BinhLuanDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.models.*;
import com.app.backend.repositories.*;
import com.app.backend.responses.community.BinhLuanResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@RequiredArgsConstructor
public class BinhLuanService implements IBinhLuanService {

    private final IBinhLuanRepository binhLuanRepository;
    private final IBaiVietRepository baiVietRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final ILuotThichBinhLuanRepository luotThichBinhLuanRepository;
    private final IBangXepHangRepository bangXepHangRepository;

    private Integer getUserLevel(Long userId) {
        return bangXepHangRepository.findByNguoiDung_Id(userId)
                .map(BangXepHang::getLevel)
                .orElse(1);
    }

    @Override
    @Transactional
    public BinhLuanResponse createComment(Long userId, BinhLuanDTO dto) throws Exception {
        NguoiDung user = nguoiDungRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        BaiViet baiViet = baiVietRepository.findById(dto.baiVietId())
                .orElseThrow(() -> new DataNotFoundException("Bài viết không tồn tại"));

        BinhLuan binhLuanCha = null;
        if (dto.binhLuanChaId() != null) {
            binhLuanCha = binhLuanRepository.findById(dto.binhLuanChaId())
                    .orElseThrow(() -> new DataNotFoundException("Bình luận cha không tồn tại"));

            // Đảm bảo comment cha thuộc cùng bài viết
            if (!binhLuanCha.getBaiViet().getId().equals(dto.baiVietId())) {
                throw new IllegalStateException("Bình luận cha không thuộc bài viết này");
            }

            // Chỉ cho phép reply 1 cấp (2-level comments)
            if (binhLuanCha.getBinhLuanCha() != null) {
                // Nếu comment cha đã là reply, thì reply vào comment gốc
                binhLuanCha = binhLuanCha.getBinhLuanCha();
            }
        }

        BinhLuan binhLuan = BinhLuan.builder()
                .baiViet(baiViet)
                .nguoiBinhLuan(user)
                .binhLuanCha(binhLuanCha)
                .noiDung(dto.noiDung())
                .build();

        BinhLuan saved = binhLuanRepository.save(binhLuan);

        // Increment comment count cho bài viết
        baiVietRepository.incrementComments(dto.baiVietId());

        return BinhLuanResponse.fromEntitySimple(saved, false, userId, getUserLevel(userId));
    }

    @Override
    @Transactional
    public BinhLuanResponse updateComment(Long userId, Long commentId, BinhLuanDTO dto) throws Exception {
        BinhLuan binhLuan = binhLuanRepository.findById(commentId)
                .orElseThrow(() -> new DataNotFoundException("Bình luận không tồn tại"));

        if (!binhLuan.getNguoiBinhLuan().getId().equals(userId)) {
            throw new IllegalStateException("Bạn không có quyền sửa bình luận này");
        }

        binhLuan.setNoiDung(dto.noiDung());
        BinhLuan saved = binhLuanRepository.save(binhLuan);

        boolean daThich = luotThichBinhLuanRepository.existsByBinhLuan_IdAndNguoiDung_Id(commentId, userId);

        return BinhLuanResponse.fromEntitySimple(saved, daThich, userId, getUserLevel(userId));
    }

    @Override
    @Transactional
    public void deleteComment(Long userId, Long commentId) throws Exception {
        BinhLuan binhLuan = binhLuanRepository.findById(commentId)
                .orElseThrow(() -> new DataNotFoundException("Bình luận không tồn tại"));

        if (!binhLuan.getNguoiBinhLuan().getId().equals(userId)) {
            throw new IllegalStateException("Bạn không có quyền xóa bình luận này");
        }

        Long postId = binhLuan.getBaiViet().getId();

        // Đếm số replies để trừ đúng
        long replyCount = binhLuanRepository.countByBinhLuanCha_IdAndBiAnFalse(commentId);

        // Xóa comment (cascade sẽ xóa replies)
        binhLuanRepository.delete(binhLuan);

        // Decrement comment count (1 cho comment + số replies)
        for (int i = 0; i <= replyCount; i++) {
            baiVietRepository.decrementComments(postId);
        }
    }

    @Override
    @Transactional(readOnly = true)
    public Page<BinhLuanResponse> getCommentsByPost(Long postId, Long currentUserId, Pageable pageable) {
        Page<BinhLuan> comments = binhLuanRepository.findRootCommentsByBaiVietId(postId, pageable);

        return comments.map(comment -> {
            boolean daThich = currentUserId != null &&
                    luotThichBinhLuanRepository.existsByBinhLuan_IdAndNguoiDung_Id(comment.getId(), currentUserId);

            Integer commentUserLevel = getUserLevel(comment.getNguoiBinhLuan().getId());

            // Lấy replies (giới hạn 3 replies đầu tiên, còn lại load qua API riêng)
            List<BinhLuan> replies = binhLuanRepository.findRepliesByParentId(comment.getId());
            List<BinhLuanResponse> replyResponses = replies.stream()
                    .limit(3)
                    .map(reply -> {
                        boolean replyDaThich = currentUserId != null &&
                                luotThichBinhLuanRepository.existsByBinhLuan_IdAndNguoiDung_Id(reply.getId(), currentUserId);
                        Integer replyUserLevel = getUserLevel(reply.getNguoiBinhLuan().getId());
                        return BinhLuanResponse.fromEntitySimple(reply, replyDaThich, currentUserId, replyUserLevel);
                    })
                    .toList();

            return BinhLuanResponse.fromEntity(comment, daThich, replyResponses, currentUserId, commentUserLevel);
        });
    }

    @Override
    @Transactional(readOnly = true)
    public Page<BinhLuanResponse> getReplies(Long commentId, Long currentUserId, Pageable pageable) {
        List<BinhLuan> allReplies = binhLuanRepository.findRepliesByParentId(commentId);

        // Manual pagination
        int start = (int) pageable.getOffset();
        int end = Math.min((start + pageable.getPageSize()), allReplies.size());

        List<BinhLuanResponse> pagedReplies = allReplies.subList(start, end).stream()
                .map(reply -> {
                    boolean daThich = currentUserId != null &&
                            luotThichBinhLuanRepository.existsByBinhLuan_IdAndNguoiDung_Id(reply.getId(), currentUserId);
                    Integer replyUserLevel = getUserLevel(reply.getNguoiBinhLuan().getId());
                    return BinhLuanResponse.fromEntitySimple(reply, daThich, currentUserId, replyUserLevel);
                })
                .toList();

        return new PageImpl<>(pagedReplies, pageable, allReplies.size());
    }

    @Override
    @Transactional
    public boolean toggleLike(Long userId, Long commentId) throws Exception {
        BinhLuan binhLuan = binhLuanRepository.findById(commentId)
                .orElseThrow(() -> new DataNotFoundException("Bình luận không tồn tại"));

        NguoiDung user = nguoiDungRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        var existingLike = luotThichBinhLuanRepository.findByBinhLuan_IdAndNguoiDung_Id(commentId, userId);

        if (existingLike.isPresent()) {
            // Unlike
            luotThichBinhLuanRepository.delete(existingLike.get());
            binhLuanRepository.decrementLikes(commentId);
            return false;
        } else {
            // Like
            LuotThichBinhLuan like = LuotThichBinhLuan.builder()
                    .binhLuan(binhLuan)
                    .nguoiDung(user)
                    .build();
            luotThichBinhLuanRepository.save(like);
            binhLuanRepository.incrementLikes(commentId);
            return true;
        }
    }

    @Override
    @Transactional
    public BinhLuanResponse hideComment(Long adminId, Long commentId) throws Exception {
        BinhLuan binhLuan = binhLuanRepository.findById(commentId)
                .orElseThrow(() -> new DataNotFoundException("Bình luận không tồn tại"));

        binhLuan.setBiAn(true);
        BinhLuan saved = binhLuanRepository.save(binhLuan);
        return BinhLuanResponse.fromEntitySimple(saved, false, adminId, getUserLevel(saved.getNguoiBinhLuan().getId()));
    }

    @Override
    @Transactional
    public BinhLuanResponse unhideComment(Long adminId, Long commentId) throws Exception {
        BinhLuan binhLuan = binhLuanRepository.findById(commentId)
                .orElseThrow(() -> new DataNotFoundException("Bình luận không tồn tại"));

        binhLuan.setBiAn(false);
        BinhLuan saved = binhLuanRepository.save(binhLuan);
        return BinhLuanResponse.fromEntitySimple(saved, false, adminId, getUserLevel(saved.getNguoiBinhLuan().getId()));
    }
}
