// Enhanced interactive features for ResTop Analytics

// Initialize when document is ready
$(document).ready(function() {
    // Add floating animation to elements
    $('.floating').each(function(index) {
        $(this).css('animation-delay', (index * 0.2) + 's');
    });
    
    // Enhanced file input styling
    $('.file-input').each(function() {
        const $input = $(this);
        const $label = $input.next('.file-input-label');
        
        $input.on('change', function(e) {
            const fileName = e.target.files[0]?.name || 'No file chosen';
            $label.html(`<i class="fas fa-file-upload"></i> ${fileName}`);
        });
    });
    
    // Smooth scrolling for anchor links
    $('a[href^="#"]').on('click', function(e) {
        e.preventDefault();
        const target = $(this.getAttribute('href'));
        if (target.length) {
            $('html, body').animate({
                scrollTop: target.offset().top - 80
            }, 800);
        }
    });
    
    // Add intersection observer for scroll animations
    if ('IntersectionObserver' in window) {
        const observer = new IntersectionObserver((entries) => {
            entries.forEach(entry => {
                if (entry.isIntersecting) {
                    entry.target.classList.add('animate-in');
                }
            });
        }, { threshold: 0.1 });
        
        document.querySelectorAll('.feature-item, .stat-card').forEach(el => {
            observer.observe(el);
        });
    }
    
    // Enhanced tooltip initialization
    $('[data-toggle="tooltip"]').tooltip({
        trigger: 'hover',
        placement: 'top',
        animation: true
    });
});

// Custom notification system
function showCustomNotification(message, type = 'info') {
    const notification = $(`
        <div class="custom-notification custom-notification-${type}">
            <i class="fas fa-${getNotificationIcon(type)}"></i>
            <span>${message}</span>
            <button class="notification-close">&times;</button>
        </div>
    `);
    
    $('#notification-container').append(notification);
    
    setTimeout(() => {
        notification.addClass('show');
    }, 100);
    
    // Auto remove after 5 seconds
    setTimeout(() => {
        hideNotification(notification);
    }, 5000);
    
    // Close on click
    notification.find('.notification-close').on('click', function() {
        hideNotification(notification);
    });
}

function getNotificationIcon(type) {
    const icons = {
        'success': 'check-circle',
        'error': 'exclamation-circle',
        'warning': 'exclamation-triangle',
        'info': 'info-circle'
    };
    return icons[type] || 'info-circle';
}

function hideNotification(notification) {
    notification.removeClass('show');
    setTimeout(() => {
        notification.remove();
    }, 300);
}